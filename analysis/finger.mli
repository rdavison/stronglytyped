open! Core

type t =
  [ `p
  | `r
  | `m
  | `i
  ]
[@@deriving sexp, bin_io, equal, compare, enumerate]

include Comparable.S with type t := t

val is_adjacent : t -> t -> bool
val roll_direction : t -> t -> [ `In | `Out ]
val to_string : t -> string

module Tuple2 : sig
  type nonrec t = t * t [@@deriving sexp, equal, compare]

  include Comparable.S with type t := t
end
