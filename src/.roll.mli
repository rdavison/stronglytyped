open! Import

type t =
  { inward : float
  ; outward : float
  }
[@@deriving sexp]

val zero : t
val add : t -> t -> t
val sub : t -> t -> t
val make : Key.t list -> bigrams:float String.Table.t -> t Hf.Table.t
val incr : t Hf.Table.t Incr.t