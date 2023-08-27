open! Import

module type S = Algorithm_intf.S

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
