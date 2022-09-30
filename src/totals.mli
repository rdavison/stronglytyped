open! Import

type t =
  { sfb : float
  ; dsfb : float
  ; rolls : float
  ; lsb : float
  ; speed : float
  ; shb : float
  ; shs : float
  }
[@@deriving sexp]

val make
  :  sfb:Sfb.t Hf.Table.t
  -> dsfb:Dsfb.t Hf.Table.t
  -> roll:Roll.t Hf.Table.t
  -> lsb:Lsb.t Hand.Table.t
  -> speed:Speed.t Hf.Table.t
  -> shb:Shb.t Hand.Table.t
  -> shs:Shs.t Hand.Table.t
  -> t

val incr : t Incr.t