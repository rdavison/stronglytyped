open! Import

type t = [ `Char of char ] [@@deriving sexp, compare, hash, equal]

let of_char c = `Char c