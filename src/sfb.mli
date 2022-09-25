open! Import

type t = float Incr.t Hf.Table.t

val make : Key.t list -> bigrams:float String.Table.t -> float
val v : bigrams:float String.Table.t -> t