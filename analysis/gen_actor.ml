open! Core
open! Bonsai
open! Bonsai.Let_syntax
module M = Computation_status

module Action = struct
  type t = |
end

let actor keyboard_score graph =
  Bonsai.actor_with_input
    ~default_model:()
    ~recv:(fun _ctx keyboard_score model (_action : Action.t) ->
      let () =
        match keyboard_score with
        | Inactive -> ()
        | Active (_keyboard, _score) -> ()
      in
      model, None)
    keyboard_score
    graph
;;
