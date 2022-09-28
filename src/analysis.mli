open! Import

type t =
  { score : Score.t
  ; layout : string
  ; pretty : Pretty.t
  }

val incr : t Incr.t