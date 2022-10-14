open! Import

type t =
  { score : float
  ; layout : string
  ; layout_pretty : string
  ; stats : Stats.t option
  }
[@@deriving sexp]

val incr : t Incr.t