open! Import

type t =
  { sfb : float
  ; dsfb : float
  ; lsb : float
  ; speed : float
  }
[@@deriving sexp]

let make ~sfb ~dsfb ~lsb ~speed = { sfb; dsfb; lsb; speed }

let incr : t Incr.t =
  Incr.map4
    Stats.sfb_total
    Stats.dsfb_total
    Stats.lsb_total
    Stats.speed_total
    ~f:(fun sfb dsfb lsb speed -> make ~sfb ~dsfb ~lsb ~speed)
;;
