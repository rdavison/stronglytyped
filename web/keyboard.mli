open! Import

include
  Stem.Keyboard_intf.S
  with type t = Stem.Keyboard.t
   and module Action = Stem.Keyboard.Action

val section_component
  :  keyboard:t Bonsai.t
  -> keyboard_inject:(Action.t list -> unit Ui_effect.t) Bonsai.t
  -> corpus:Corpus.t Bonsai.t
  -> theme:[ `Dark | `Light ] Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
