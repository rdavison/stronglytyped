open! Import

type t = unit [@@deriving sexp, compare]

val worst : t
