open! Import

type 'a info =
  { s1 : 'a String.Table.t * 'a
  ; s2 : 'a String.Table.t * 'a
  ; s3 : 'a String.Table.t * 'a
  ; s4 : 'a String.Table.t * 'a
  ; s5 : 'a String.Table.t * 'a
  ; s6 : 'a String.Table.t * 'a
  ; s7 : 'a String.Table.t * 'a
  ; s8 : 'a String.Table.t * 'a
  ; s9 : 'a String.Table.t * 'a
  ; singles : 'a Char.Table.t * 'a
  ; triples : 'a String.Table.t * 'a
  ; words : 'a String.Table.t * 'a
  }
[@@deriving sexp]

type t =
  { freqs : float info
  ; counts : Bignum.t info
  }
[@@deriving sexp]

val load_corpus : string -> t
val of_string : string -> t

module Lookup : sig
  val freq1 : Code.t -> data:float Char.Table.t -> float
  val freq2 : Code.t * Code.t -> data:float String.Table.t -> float
  val freq3 : Code.t * Code.t * Code.t -> data:float String.Table.t -> float
end
