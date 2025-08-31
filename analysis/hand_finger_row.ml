open! Core

module T = struct
  type t = Hand.t * Finger.t * Int.t [@@deriving sexp, compare, equal]
end

include T
include Comparable.Make (T)

let to_string (h, f, r) = sprintf "%s%s%d" (Hand.to_string h) (Finger.to_string f) r

module Tuple2 = struct
  module T = struct
    type t = T.t * T.t [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T)
end
