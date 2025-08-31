open! Core

module T = struct
  type t = Hand.t * Int.t [@@deriving sexp, compare, equal]
end

include T
include Comparable.Make (T)

let to_string (h, r) = sprintf "%s%d" (Hand.to_string h) r

module Tuple2 = struct
  module T = struct
    type t = T.t * T.t [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T)
end
