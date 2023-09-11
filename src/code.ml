open! Import

type t = [ `Char of char ] [@@deriving sexp, compare, hash, equal]

let of_char c = `Char c

let to_string = function
  | `Char c -> String.of_array [| c |]
;;
