open! Import

type t =
  { stats : Stats.t
  ; totals : Totals.t
  ; score : Score.t
  ; layout : string
  ; pretty : Pretty.t
  }

val incr : t Incr.t