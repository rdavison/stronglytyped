open! Core
open! Bonsai_web

include
  Analysis.Keyboard_intf.S
  with type t = Analysis.Keyboard.t
   and module Action = Analysis.Keyboard.Action

val section_component
  :  keyboard:t Bonsai.t
  -> keyboard_inject:(Action.t list -> unit Ui_effect.t) Bonsai.t
  -> corpus:Corpus.t Bonsai.t
  -> theme:[ `Dark | `Light ] Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
