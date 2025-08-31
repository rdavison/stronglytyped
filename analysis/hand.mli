open! Core

type t =
  [ `l
  | `r
  ]
[@@deriving sexp, equal, compare, enumerate]

include Comparable.S with type t := t

val to_string : t -> string

module Tuple2 : sig
  type nonrec t = t * t [@@deriving sexp, equal, compare]

  include Comparable.S with type t := t
end
