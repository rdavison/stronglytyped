open! Import

module type S = Stats_intf.S

module Make (Incr : Incremental.S) (Layout : Layout.S with module Incr = Incr) :
  S with module Incr = Incr and module Layout = Layout
