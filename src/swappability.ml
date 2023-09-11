open! Import

type t =
  | Tower
  | Single
  | Noswap
[@@deriving sexp, compare, hash, equal]

let of_int = function
  | 0 -> Noswap
  | 1 -> Single
  | 2 -> Tower
  | n -> failwithf "BUG: Unhandled case: %d" n ()
;;
