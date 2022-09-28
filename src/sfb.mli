open! Import

type t = float

val make : Key.t list -> bigrams:float String.Table.t -> t
val table : bigrams:float String.Table.t -> t Incr.t Hf.Table.t