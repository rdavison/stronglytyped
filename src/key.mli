open! Import

type t =
  { code : Code.t
  ; rc : int * int
  ; hand : Hand.t
  ; finger : Finger.t
  }
[@@deriving sexp, compare, hash, equal]

include Comparable.S with type t := t
include Hashable.S with type t := t

val make : int -> char -> t
val all_arr_incr : t Incr.t array
val all_incr_set : Set.t Incr.t
val all_incr_map : unit Map.t Incr.t
val dist : ?stagger:Stagger.t -> t -> t -> float
