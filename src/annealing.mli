open! Import

val run
  :  Layout.t
  -> corpus:Corpus.t
  -> score:(Stats.t -> Score.t Incr.t)
  -> float * Stats.t * Score.t Incr.t * Layout.save_state
