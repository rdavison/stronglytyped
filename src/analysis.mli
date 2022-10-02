open! Import

type t =
  { score : float
  ; layout : string
  ; pretty : Pretty.t
  }

val incr : t Incr.t