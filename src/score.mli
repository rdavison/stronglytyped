open! Import

type t = float

val make : Stats.t -> t
val incr : config:Config.t -> t Incr.t