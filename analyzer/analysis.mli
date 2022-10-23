open! Import

type t =
  { score : float
  ; layout : string
  ; layout_pretty : string
  ; stats : float Stats.t option
  }
[@@deriving sexp, compare]

val empty : t
val make_incr : float Incr.t -> t Incr.t