open! Import

type t = char Incr.Var.t array

val v : t
val swap : int -> int -> unit
val length : int