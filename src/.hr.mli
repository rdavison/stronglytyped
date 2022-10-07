open! Import

type t = Hand.t * int [@@deriving sexp, compare, hash, equal]

include Comparable.S with type t := t
include Hashable.S with type t := t

val all : t list
val to_string : t -> string