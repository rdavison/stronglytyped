open! Import

type t =
  { score : float
  ; layout : string
  ; layout_pretty : string
  }
[@@deriving sexp]

val incr : t Incr.t