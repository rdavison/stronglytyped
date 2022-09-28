open! Import

type t =
  { score : Score.t
  ; pretty : Pretty.t
  }

val incr : t Incr.t