open! Import

val all : (string * string) list
val set : [ `Name of string | `Layout of string ] -> unit
val best : (float * string) list Incr.t
val set_best : (float * string) list -> unit
val set_next : string -> unit