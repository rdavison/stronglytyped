open! Import

val all : (string * string) list
val set : Root.t array -> [ `Name of string | `Layout of string ] -> unit
val best : (float * string) list Incr.t
val set_best : (float * string) list -> root:Root.t array -> unit
val set_next : string -> root:Root.t array -> unit
