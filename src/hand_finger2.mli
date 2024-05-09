open! Import

type t = Hand.t * Finger.t * Finger.t [@@deriving sexp, compare, hash, equal]

include Comparable.S with type t := t
include Hashable.S with type t := t
module Infix : Comparable.Infix with type t := t

val all : t list
val to_string : t -> string
val of_string : string -> t option
