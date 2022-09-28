open! Import

type t = char Incr.Var.t

val qwerty : string
val all : t array
val swap : int -> int -> unit
val rebase : string -> unit
val length : int