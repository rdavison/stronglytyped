open! Import

type t =
  | Tower
  | Single
  | Noswap
[@@deriving sexp, compare, hash, equal]

val of_int : int -> t
