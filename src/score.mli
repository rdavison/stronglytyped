open! Import

module type S = Score_intf.S

module Make
    (Incr : Incremental.S)
    (Layout : Layout.S with module Incr = Incr)
    (Stats : Stats.S with module Incr = Incr and module Layout = Layout) :
  S with module Incr = Incr and module Layout = Layout and module Stats = Stats
