open! Import

type t = char Incr.Var.t

val all : t array
val swap : int -> int -> unit
val rebase : string -> unit
val scramble : int -> unit
val length : int
val layout : string Incr.t
val layout_pretty : string Incr.t