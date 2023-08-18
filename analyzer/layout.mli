open! Import

val all : (string * string) list
val set : Root.t -> [ `Name of string | `Layout of string ] -> unit
val best : (float * string) list Incr.t
val set_best : (float * string) list -> root:Root.t -> unit
val set_next : string -> root:Root.t -> unit
