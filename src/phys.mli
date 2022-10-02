open! Import

type 'a t = ((int * int) * 'a Incr.t) list

val sort : 'a t -> 'a t
val pairs : 'a t -> ('a * 'a) t
val dedup : 'a t -> 'a t
val incr : 'a t -> 'a list Incr.t
val make : int list -> int list -> Key.t list
val columns : int list -> Key.t list
val rows : int list -> Key.t list
val hand : Hand.t -> Key.t list
val hf : Hf.t -> Key.t list
val hr : Hr.t -> Key.t list
val finger : Finger.t -> Key.t list