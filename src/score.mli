open! Import

type t = float

val make
  :  sfb:Sfb.t Hf.Table.t
  -> dsfb:Dsfb.t Hf.Table.t
  -> roll:Roll.t Hf.Table.t
  -> lsb:Lsb.t Hand.Table.t
  -> c_sfb:float
  -> c_dsfb:float
  -> c_roll:float
  -> c_lsb:float
  -> t

val incr : t Incr.t