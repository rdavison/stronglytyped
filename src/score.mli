open! Import

type t = float

val make
  :  sfb:Sfb.t Hf.Table.t
  -> dsfb:Dsfb.t Hf.Table.t
  -> roll:Roll.t Hf.Table.t
  -> lsb:Lsb.t Hand.Table.t
  -> keyfreq:Keyfreq.t Hf.Table.t
  -> w_sfb:float
  -> w_dsfb:float
  -> w_keyfreq:float Finger.Table.t
  -> w_rolls:float
  -> w_lsbs:float
  -> t

val incr : t Incr.t