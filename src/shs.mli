open! Import

type t = float

val make : Key.t list -> skipgrams:float String.Table.t -> t
val incr : t Hand.Table.t Incr.t
