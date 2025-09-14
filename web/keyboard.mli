open! Core
open! Bonsai_web
module Key := Analysis.Key
module Keyboard := Analysis.Keyboard
module Corpus := Analysis.Corpus

val component
  :  Keyboard.t Bonsai.t
  -> Corpus.t Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
