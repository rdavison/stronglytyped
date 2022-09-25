open! Import

type t = [ `Char of char ] [@@deriving sexp, compare, hash, equal]

val make : char -> t