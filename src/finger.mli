open! Import

type t =
  [ `P
  | `R
  | `M
  | `I
  ]
[@@deriving sexp, compare, hash, equal]

include Comparable.S with type t := t
include Hashable.S with type t := t

val to_int : t -> int
val of_int : int -> t
val all : t list
