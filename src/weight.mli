open! Import

type t = float Incr.t Hf.Table.t

val make : Key.t list -> monograms:float Char.Table.t -> float
val v : monograms:float Char.Table.t -> t