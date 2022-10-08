open! Import

type t =
  { score : float
  ; layout : string
  ; layout_pretty : string
  ; temperature : float
  ; cooling_factor : float
  ; best : (float * string) list
  }
[@@deriving sexp]

val incr : t Incr.t