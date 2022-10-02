open! Import

type t =
  { sfb : float
  ; dsfb : float
  ; lsb : float
  ; speed : float
  }
[@@deriving sexp]

val make : sfb:float -> dsfb:float -> lsb:float -> speed:float -> t
val incr : t Incr.t