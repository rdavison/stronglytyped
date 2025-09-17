open! Core
open! Bonsai
open! Bonsai.Let_syntax

module One_per_frame_after_display = struct
  module Action = struct
    type 'action t =
      | Enqueue_many of 'action list
      | Tick
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
    let%arr inject = inject in
    fun eff -> inject (Enqueue_many eff)
  ;;
end

type t = Key.t Key.Id.Map.t [@@deriving sexp, bin_io, equal, compare]

module Action = struct
  type t =
    | Swap of (Key.Id.t * Key.Id.t)
    | Random_swap
  [@@deriving sexp, compare, equal]
end

let state_machine graph : t Bonsai.t * (Action.t list -> unit Ui_effect.t) Bonsai.t =
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
      let a, b =
        match action with
        | Random_swap -> Key.Id.rand2 ()
        | Swap ab -> ab
      in
      let a_key, a_set = Map.find_exn state_machines_by_id a in
      let b_key, b_set = Map.find_exn state_machines_by_id b in
      Ui_effect.all_unit [ a_set (Set b_key.kc); b_set (Set a_key.kc) ]
  in
  let inject = One_per_frame_after_display.component inject graph in
  keyboard, inject
;;
