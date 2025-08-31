open! Core

module T = struct
  type t =
    [ `p
    | `r
    | `m
    | `i
    ]
  [@@deriving sexp, equal, compare, enumerate]

  let is_adjacent a b =
    match a, b with
    | `p, `r | `r, `p | `r, `m | `m, `r | `m, `i | `i, `m -> true
    | _, _ -> false
  ;;

  let roll_direction a b =
    match a, b with
    | `p, (`r | `m | `i) | `r, (`m | `i) | `m, `i -> `In
    | _, _ -> `Out
  ;;
end

include T
include Comparator.Make (T)

let to_string = function
  | `p -> "P"
  | `r -> "R"
  | `m -> "M"
  | `i -> "I"
;;

module Tuple2 = struct
  module T = struct
    type t = T.t * T.t [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T)
end
