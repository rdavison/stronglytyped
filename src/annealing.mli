open! Import

val run
  :  Layout.t
  -> corpus:Corpus.t
  -> weights:Score.weights
  -> float * Layout.save_state
