open! Import

type t = float

val make : Key.t list -> monograms:float Char.Table.t -> t
val table : monograms:float Char.Table.t -> t Incr.t Hf.Table.t