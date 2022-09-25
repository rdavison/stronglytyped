open! Import
open! Incr

type t = Key.t * Key.t

val make : Key.t -> Key.t -> t
val all : t Incr.t array