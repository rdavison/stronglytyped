open! Import

type t = Hand.t * Finger.t [@@deriving sexp, compare, equal, enumerate, bin_io]

include Comparable.S_binable with type t := t

val to_string : t -> string

module Tuple2 : sig
  type nonrec t = t * t [@@deriving sexp, equal, compare]

  include Comparable.S with type t := t
end
