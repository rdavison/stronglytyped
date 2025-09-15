open! Core
open! Bonsai_web

val section_component
  :  keyboard:Analysis.Keyboard.t Bonsai.t
  -> keyboard_inject:(Analysis.Keyboard.Action.t -> unit Ui_effect.t) Bonsai.t
  -> corpus:Analysis.Corpus.t Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
