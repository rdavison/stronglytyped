open! Core

module T = struct
  type t =
    [ `l
    | `r
    ]
  [@@deriving sexp, equal, compare, enumerate]
end

include T
include Comparable.Make (T)

let to_string = function
  | `l -> "L"
  | `r -> "R"
;;

module Tuple2 = struct
  module T = struct
    type t = T.t * T.t [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T)
end
