open! Import

type t = float

val make : Stats.t -> w_sfb:t -> w_dsfb:t -> w_weight:t Finger.Table.t -> w_rolls:t -> t
val incr : t Incr.t