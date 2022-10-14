open! Import

type t = Hand.t * Finger.t [@@deriving sexp, compare, hash, equal]

include Comparable.S with type t := t
include Hashable.S with type t := t

val to_int : t -> int
val of_int : int -> t
val all : t list
val to_string : t -> string