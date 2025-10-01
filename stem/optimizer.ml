open! Import

type t =
  | Random
  | Greedy
[@@deriving sexp, compare, equal, enumerate, bin_io]

let default = Random
