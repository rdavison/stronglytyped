open! Import

type t =
  { sfb : float Hf.Table.t
  ; dsfb : float Hf.Table.t
  ; rolls : Roll.t Hf.Table.t
  ; lsbs : Lsb.t Hand.Table.t
  ; weight : float Hf.Table.t
  }
[@@deriving sexp]

val make
  :  float Hf.Table.t
  -> float Hf.Table.t
  -> Roll.t Hf.Table.t
  -> Lsb.t Hand.Table.t
  -> float Hf.Table.t
  -> t

val incr : t Incr.t