open! Core

module type S = sig
  module Maps : sig
    type 'a t =
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
  end

  module Bigrams : sig
    type 'a t =
      { ab : 'a
      ; aba : 'a
      ; axc : 'a
      ; vbcv : 'a
      }
    [@@deriving sexp, equal]
  end

  type t =
    { count : int Maps.t
    ; freq : float Maps.t
    }
  [@@deriving sexp, equal]

  val empty : t
  val of_string : string -> t
  val bigrams : t -> string -> float Bigrams.t
  val foo : t
end
