open! Import
open! Incr

type t =
  { sfb : float
  ; dsfb : float
  ; lsb : float
  ; speed : float
  }
[@@deriving sexp]

let make ~sfb ~dsfb ~lsb ~speed = { sfb; dsfb; lsb; speed }

let incr : t Incr.t =
  map4 Sf.B.total Sf.S.total Sf.Lsb.total Sf.Speed.total ~f:(fun sfb dsfb lsb speed ->
      make ~sfb ~dsfb ~lsb ~speed)
;;
