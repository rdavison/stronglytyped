open! Import

type t = float

val make
  :  Stats.t
  -> w_sfb:float
  -> w_dsfb:float
  -> w_weight:float Finger.Table.t
  -> w_rolls:float
  -> w_lsbs:float
  -> t

val incr : t Incr.t