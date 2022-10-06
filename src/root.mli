open! Import

type t = char Incr.Var.t

val default : string
val apply_layout : [ `Name of string | `Layout of string ] -> unit
val all : t array
val swap : int -> int -> unit
val rebase : string -> unit
val scramble : int -> unit
val length : int
val layout : string Incr.t
val layout_pretty : string Incr.t