open! Core
open! Bonsai_web_proc
module Keyboard = Stronglytyped_analysis.Keyboard
module Corpus = Stronglytyped_analysis.Corpus

val counter : int -> (int -> string) -> (int * Vdom.Node.t) Computation.t

val component
  :  Keyboard.t Value.t
  -> Corpus.t Value.t
  -> int Value.t
  -> Vdom.Node.t Computation.t
