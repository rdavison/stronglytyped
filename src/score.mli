open! Import

type t = float

val make
  :  totals:Totals.t
  -> c_sfb:float
  -> c_dsfb:float
  -> c_roll:float
  -> c_lsb:float
  -> c_speed:float
  -> c_shb:float
  -> c_shs:float
  -> t

val incr : t Incr.t