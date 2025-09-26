open! Core

type t = Key.Id.t list list [@@deriving sexp, equal, compare]

val ansi : t
val k10x3 : t
