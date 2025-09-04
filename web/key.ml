open! Core
open! Bonsai_web_proc
open! Bonsai.Let_syntax
module Keycode = Stronglytyped_analysis.Keycode
module Key = Stronglytyped_analysis.Key

module Action = struct
  type t = Set of Keycode.t [@@deriving sexp, equal]
end

let state_machine id =
  Bonsai.state_machine0
    (module Keycode)
    (module Action)
    ~default_model:(Key.Id.default_kc id)
    ~apply_action:(fun ~inject:_ ~schedule_event:_ _model action ->
      match action with
      | Set kc -> kc)
;;
