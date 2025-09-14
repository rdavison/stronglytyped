open! Core
open! Bonsai
open! Bonsai.Let_syntax

type t = Key.t Key.Id.Map.t [@@deriving sexp, bin_io, equal, compare]

module Action = struct
  type t =
    | Swap of (Key.Id.t * Key.Id.t)
    | Random_swap
  [@@deriving sexp, compare, equal]
end

let state_machine graph : t Bonsai.t * (Action.t -> unit Ui_effect.t) Bonsai.t =
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
  keyboard, inject
;;
