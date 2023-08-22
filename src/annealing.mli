open! Import

val run
  :  Layout.t
  -> corpus:Corpus.t
  -> config:Score.config
  -> float * Stats.t * Score.t Incr.t * Layout.save_state
