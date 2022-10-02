open! Import

type t = float

val sfb : t Hf.Map.t Incr.t
val sfb_total : t Incr.t
val dsfb : t Hf.Map.t Incr.t
val dsfb_total : t Incr.t
val speed : t Hf.Map.t Incr.t
val speed_total : t Incr.t
val lsb : t Hand.Map.t Incr.t
val lsb_total : t Incr.t
val roll : t Hr.Map.t Incr.t
val roll_total : t Incr.t
val incr : t Incr.t