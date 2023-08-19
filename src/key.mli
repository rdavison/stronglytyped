open! Import

type t =
  { finger : Finger.t
  ; hand : Hand.t
  ; x : float
  ; y : float
  ; col : int
  ; row : int
  ; layer : int
  ; layer_trigger : int option
  ; modifier : bool
  ; swappable : bool
  ; locked_to : int list
  }
[@@deriving sexp, compare, hash, equal]

include Comparable.S with type t := t
include Hashable.S with type t := t

module T2 : sig
  type nonrec t = t * t [@@deriving sexp, compare, hash]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module T3 : sig
  type nonrec t = t * t * t [@@deriving sexp, compare, hash]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

val hand_finger : t -> Hand_finger.t
