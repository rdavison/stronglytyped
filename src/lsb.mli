open! Import

type t = float [@@deriving sexp]

val incr : t Hand.Table.t Incr.t