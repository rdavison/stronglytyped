open! Core

module T = struct
  type t = Key.t * Key.t [@@deriving sexp, equal, compare]
end

include T
include Comparable.Make (T)
