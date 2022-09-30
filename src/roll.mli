open! Import

type t =
  { inward : float
  ; outward : float
  }
[@@deriving sexp]

include Container.Summable with type t := t

val make : Key.t list -> bigrams:float String.Table.t -> t Hf.Table.t
val incr : t Hf.Table.t Incr.t