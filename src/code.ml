open! Import

type t = [ `Char of char ] [@@deriving sexp, compare, hash, equal]

let make c = `Char c