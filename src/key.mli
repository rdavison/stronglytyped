open! Import

type t =
  { code : Code.t
  ; rc : int * int
  ; hand : Hand.t
  ; finger : Finger.t
  }
[@@deriving sexp, compare, hash, equal]

val make : int -> char -> t
val all : t Incr.t array
