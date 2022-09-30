open! Import
open! Incr

type t =
  { sfb : float Hf.Table.t
  ; dsfb : float Hf.Table.t
  ; rolls : Roll.t Hf.Table.t
  ; lsbs : Lsb.t Hand.Table.t
  ; weight : float Hf.Table.t
  }
[@@deriving sexp]

let make sfb dsfb rolls lsbs weight = { sfb; dsfb; rolls; lsbs; weight }
let incr : t Incr.t = map5 Sfb.incr Dsfb.incr Roll.incr Lsb.incr Keyfreq.incr ~f:make
