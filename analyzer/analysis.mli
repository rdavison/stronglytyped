open! Import

type t =
  { score : float
  ; layout : string
  ; layout_pretty : string
  ; stats : Stats.t option
  }
[@@deriving sexp, compare]

val empty : t
val incr : t Incr.t