open! Core

type 'a maps =
  { a : 'a Char.Map.t
  ; ab : 'a String.Map.t
  ; aba : 'a String.Map.t
  ; abxab : 'a String.Map.t
  ; abxba : 'a String.Map.t
  ; abc : 'a String.Map.t
  ; abcd : 'a String.Map.t
  ; axc : 'a String.Map.t
  ; vbcv : 'a String.Map.t
  }
[@@deriving sexp, equal]

type 'a bigrams =
  { ab : 'a
  ; aba : 'a
  ; axc : 'a
  ; vbcv : 'a
  }
[@@deriving sexp, equal]

type t =
  { count : int maps
  ; freq : float maps
  }
[@@deriving sexp, equal]

val empty : t
val of_string : string -> t
val bigrams : t -> string -> float bigrams
val foo : t
