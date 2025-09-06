open! Core
open! Bonsai_web
module Key := Stronglytyped_analysis.Key
module Keyboard := Stronglytyped_analysis.Keyboard
module Corpus := Stronglytyped_analysis.Corpus

module Action : sig
  type t =
    | Swap of (Key.Id.t * Key.Id.t)
    | Random_swap
  [@@deriving sexp, equal, compare]
end

val state_machine
  :  Bonsai.graph
  -> Keyboard.t Bonsai.t * (Action.t -> unit Ui_effect.t) Bonsai.t

val component
  :  Keyboard.t Bonsai.t
  -> Corpus.t Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
