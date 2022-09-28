open! Import

type t = float

val make : Stats.t -> t
val incr : t Incr.t