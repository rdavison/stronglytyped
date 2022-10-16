open! Import

type t = char Incr.Var.t

val all : t array
val swap : ?on_swap:(int * int -> unit) -> int -> int -> unit
val rebase : string -> unit
val scramble : ?on_swap:(int * int -> unit) -> int -> unit
val length : int
val layout : string Incr.t
val layout_pretty : string Incr.t
val reverse_lookup_table : int Char.Map.t Incr.t