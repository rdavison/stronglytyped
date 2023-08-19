open! Import

type t = [ `Char of char ] [@@deriving sexp, compare, hash, equal]

val of_char : char -> t
val to_string : t -> string
