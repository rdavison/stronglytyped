open! Core
open! Bonsai_web
module Keyboard := Stronglytyped_analysis.Keyboard
module Corpus := Stronglytyped_analysis.Corpus

val counter
  :  int
  -> (int -> string)
  -> Bonsai.graph
  -> int Bonsai.t * Vdom.Node.t Bonsai.t

val component
  :  Keyboard.t Bonsai.t
  -> Corpus.t Bonsai.t
  -> int Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
