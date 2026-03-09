open! Import

module One_per_frame_after_display = struct
  module Action = struct
    type 'action t =
      | Enqueue_many of 'action list
      | Tick
      | Cancel
  end

  type 'action t =
    { queue : 'action Queue.t
    ; running : bool
    }

  let component inject graph =
    let state, inject =
      Bonsai.state_machine_with_input
        ~default_model:{ queue = Queue.create (); running = false }
        ~apply_action:(fun ctx inject { queue; running } action ->
          match action with
          | Action.Enqueue_many effs ->
            Queue.enqueue_all queue effs;
            { queue; running = running || not (Queue.is_empty queue) }
          | Cancel -> { queue = Queue.create (); running = false }
          | Tick ->
            (match Queue.dequeue queue with
             | None -> { queue; running = false }
             | Some action ->
               (match inject with
                | Inactive -> { queue; running = false }
                | Active inject ->
                  Bonsai.Apply_action_context.schedule_event ctx (inject action);
                  { queue; running = true })))
        inject
        graph
    in
    let () =
      Bonsai.Edge.after_display
        (let%arr inject = inject
         and state = state in
         if state.running then inject Action.Tick else Ui_effect.Ignore)
        graph
    in
    let enqueue_many =
      let%arr inject = inject in
      fun eff -> inject (Enqueue_many eff)
    in
    let cancel =
      let%arr inject = inject in
      inject Cancel
    in
    enqueue_many, cancel
  ;;
end

type t = Key.t Key.Id.Map.t [@@deriving sexp, bin_io, equal, compare]

let swap (t : t) (id_a, id_b) =
  Option.both (Map.find t id_a) (Map.find t id_b)
  |> Option.map ~f:(fun (a, b) ->
    t
    |> Map.set ~key:id_a ~data:{ a with kc = b.kc }
    |> Map.set ~key:id_b ~data:{ b with kc = a.kc })
;;

let to_string (t : t) =
  let arrangement = Arrangement.k10x3 in
  let key id =
    let keycode =
      match
        let%map.Option key = Map.find t id in
        key.Key.kc
      with
      | None -> Key.Id.default_kc id
      | Some kc -> kc
    in
    Keycode.to_string_lower keycode
  in
  let row row = row |> List.map ~f:key |> String.concat ~sep:"" in
  let keyboard = arrangement |> List.map ~f:row |> String.concat ~sep:"" in
  keyboard
;;

module Action = struct
  type t =
    | Swap of (Key.Id.t * Key.Id.t)
    | Random_swap
    | Overwrite of Keycode.t Key.Id.Map.t
  [@@deriving sexp, compare, equal]
end

let state_machine graph
  : t Bonsai.t * (Action.t list -> unit Ui_effect.t) Bonsai.t * unit Ui_effect.t Bonsai.t
  =
  let ids = Key.Id.Set.of_list Key.Id.all_var in
  let state_machines_by_id =
    let map = Key.Id.Map.of_key_set ids ~f:Key.state_machine in
    Bonsai.all_map map graph
  in
  let keyboard : t Bonsai.t =
    Bonsai.assoc
      (module Key.Id)
      state_machines_by_id
      ~f:(fun _key state_machine _graph ->
        let%arr state, _inject = state_machine in
        state)
      graph
  in
  let inject =
    let%arr state_machines_by_id = state_machines_by_id in
    fun (action : Action.t) ->
      let swap (a, b) =
        let a_key, a_set = Map.find_exn state_machines_by_id a in
        let b_key, b_set = Map.find_exn state_machines_by_id b in
        Ui_effect.all_parallel_unit [ a_set (Set b_key.kc); b_set (Set a_key.kc) ]
      in
      match action with
      | Random_swap -> swap (Key.Id.rand2 ())
      | Swap ab -> swap ab
      | Overwrite mapping ->
        Map.to_alist state_machines_by_id
        |> List.fold ~init:(Ui_effect.return ()) ~f:(fun eff (id, (_key, set)) ->
          let keycode =
            match Map.find mapping id with
            | None -> Key.Id.default_kc id
            | Some keycode -> keycode
          in
          let%bind.Ui_effect.Par () = eff in
          set (Set keycode))
  in
  let inject, cancel = One_per_frame_after_display.component inject graph in
  keyboard, inject, cancel
;;

let all_swaps (t : t Bonsai.t) graph =
  let keys = Bonsai.Map.keys t graph in
  let id_pair_set =
    let prod = Bonsai.Set.cartesian_product keys keys graph in
    Bonsai.Set.filter prod ~f:(fun (id_a, id_b) -> not (Key.Id.equal id_a id_b)) graph
  in
  let id_pair_set =
    let to_ab a b = if Key.Id.compare a b <= 0 then a, b else b, a in
    let res =
      Bonsai.Set.unordered_fold
        id_pair_set
        ~init:Key.Id.Pair.Set.empty
        ~add:(fun acc (a, b) -> Core.Set.add acc (to_ab a b))
        ~remove:(fun acc (a, b) -> Core.Set.remove acc (to_ab a b))
        graph
    in
    let count =
      let%arr res = res in
      printf "id_pair_set: %d\n" (Core.Set.length res)
    in
    let res =
      let%arr () = count
      and res = res in
      res
    in
    res
  in
  let res = Bonsai.Map.of_set id_pair_set graph in
  let res =
    Bonsai.assoc
      (module Key.Id.Pair)
      res
      ~f:(fun id_pair _unit _graph ->
        let%arr id_pair = id_pair
        and t = t in
        let after = swap t id_pair |> Option.value_exn in
        (* let before = t in *)
        (* printf "before: %s\n" (to_string before); *)
        (* printf *)
        (*   " after: %s\n" *)
        (*   (match after with *)
        (*    | None -> "None" *)
        (*    | Some after -> to_string after); *)
        after)
      graph
  in
  res
;;
