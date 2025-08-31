open! Core
open! Bonsai_web
module Key = Stronglytyped_analysis.Key
module Keyboard = Stronglytyped_analysis.Keyboard
module Corpus = Stronglytyped_analysis.Corpus

module Action : sig
  type t =
    | Swap of (Key.Id.t * Key.Id.t)
    | Random_swap
  [@@deriving sexp, equal, compare]
end

val state_machine : (Keyboard.t * (Action.t -> unit Ui_effect.t)) Computation.t
val component : Keyboard.t Value.t -> Corpus.t Value.t -> Vdom.Node.t Computation.t
