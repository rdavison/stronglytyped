open! Import

type t = float

val make : Key.t list -> monograms:float Char.Table.t -> t
val incr : t Hf.Table.t Incr.t
