open! Import

type t = (int * int) * Key.t Incr.t

val make : rows:int list -> cols:int list -> t list
val hf : Hf.t -> t list
val hr : Hand.t * int -> t list
val hand : Hand.t -> t list
val finger : Finger.t -> t list
val rows : int list -> t list
val columns : int list -> t list
val pairs : t list -> (t * t) list
val dedup : t list -> t list
val dedup2 : (t * t) list -> (t * t) list
val unique_fingers2 : (t * t) list -> (t * t) list
val symmetric2 : (t * t) list -> (t * t) list
val incr : t list -> Key.t list Incr.t
val incr2 : (t * t) list -> (Key.t * Key.t) list Incr.t
