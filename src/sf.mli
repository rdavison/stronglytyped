open! Import

type t = float

val make : Key.t list -> bigrams:float String.Table.t -> t

module B : sig
  val incr : t Hf.Map.t Incr.t
  val total : t Incr.t
end

module S : sig
  val incr : t Hf.Map.t Incr.t
  val total : t Incr.t
end

module Speed : sig
  val incr : t Hf.Map.t Incr.t
  val total : t Incr.t
end

module Lsb : sig
  val incr : t Hand.Map.t Incr.t
  val total : t Incr.t
end

module Keyfreq : sig
  val incr : t Hf.Map.t Incr.t
  val total : t Incr.t
end

module Roll : sig
  type t =
    { inward : float
    ; outward : float
    }
  [@@deriving sexp]

  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val incr : t Hr.Map.t Incr.t
  val total : t Incr.t
end