open! Import

type t = (int * int) * Key.t Incr.t

val incr : t list -> Key.t list Incr.t
val incr' : (t * t) list -> (Key.t * Key.t) list Incr.t
val dedup : t list -> t list
val dedup' : (t * t) list -> (t * t) list
val unique_fingers' : (t * t) list -> (t * t) list
val symmetric' : (t * t) list -> (t * t) list
val pairs : t list -> (t * t) list
val make : rows:int list -> cols:int list -> t list
val columns : int list -> t list
val rows : int list -> t list
val hand : Hand.t -> t list
val hf : Hf.t -> t list
val hr : Hr.t -> t list
val finger : Finger.t -> t list
