open! Import

type t =
  { score : float
  ; layout : string
  ; layout_pretty : string
  }

val incr : t Incr.t