open! Core

type t =
  [ `Alpha of char
  | `Sym of char * char
  | `Legend of string
  | `Power
  ]
[@@deriving sexp, bin_io, equal, compare]

val to_string_upper : t -> string
val to_string_lower : t -> string
