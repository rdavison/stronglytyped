open! Import

type t =
  { s1 : float String.Table.t
  ; s2 : float String.Table.t
  ; s3 : float String.Table.t
  ; s4 : float String.Table.t
  ; s5 : float String.Table.t
  ; s6 : float String.Table.t
  ; s7 : float String.Table.t
  ; s8 : float String.Table.t
  ; s9 : float String.Table.t
  ; singles : float Char.Table.t
  ; triples : float String.Table.t
  }
[@@deriving sexp]

val load_corpus : string -> t
val of_string : string -> t

module Lookup : sig
  val freq1 : Code.t -> data:float Char.Table.t -> float
  val freq2 : Code.t * Code.t -> data:float String.Table.t -> float
  val freq3 : Code.t * Code.t * Code.t -> data:float String.Table.t -> float
end
