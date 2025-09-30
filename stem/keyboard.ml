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
        let foo2 =
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
        foo2
  in
  let inject, cancel = One_per_frame_after_display.component inject graph in
  keyboard, inject, cancel
;;
