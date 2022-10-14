open! Import

type t =
  [ `Matrix
  | `Colstag
  | `Typewriter
  | `HHKB
  ]

val default : t
val to_string : t -> string
val row_offset : t -> int -> float
val var : t Incr.Var.t
val incr : t Incr.t