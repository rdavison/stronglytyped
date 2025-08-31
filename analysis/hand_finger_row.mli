open! Core

type t = Hand.t * Finger.t * Int.t [@@deriving sexp, compare, equal]

include Comparable.S with type t := t

val to_string : t -> string

module Tuple2 : sig
  type nonrec t = t * t [@@deriving sexp, equal, compare]

  include Comparable.S with type t := t
end
