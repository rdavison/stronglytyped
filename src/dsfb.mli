open! Import

type t = float

val make : Key.t list -> skipgrams:float String.Table.t -> t
val table : skipgrams:float String.Table.t -> t Incr.t Hf.Table.t