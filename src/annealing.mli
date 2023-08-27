open! Import

module type S = sig
  module Incr : Incremental.S
  module Layout : Layout.S
  module Stats : Stats.S
  module Score : Score.S

  val run
    :  Layout.t
    -> corpus:Corpus.t
    -> score:(Stats.t -> Score.t Incr.t)
    -> float * Stats.t * Score.t Incr.t * Layout.save_state
end

module Make
    (Incr : Incremental.S)
    (Layout : Layout.S with module Incr = Incr)
    (Stats : Stats.S with module Incr = Incr and module Layout = Layout)
    (Score : Score.S
               with module Incr = Incr
                and module Layout = Layout
                and module Stats = Stats) :
  S
    with module Incr = Incr
     and module Layout = Layout
     and module Stats = Stats
     and module Score = Score
