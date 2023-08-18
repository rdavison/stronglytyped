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

(* val make : int -> char -> t *)

(* val all_arr_incr : t Incr.t array
   val all_list_incr : t Incr.t list
   val all_incr_set : Set.t Incr.t
   val all_incr_map : unit Map.t Incr.t *)
val direction : t -> t -> [ `I | `O ]
val dist : t -> t -> stagger:Stagger.t -> float
val slope : t -> t -> stagger:Stagger.t -> float
