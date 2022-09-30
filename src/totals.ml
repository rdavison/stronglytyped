open! Import
open! Incr

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

let make ~sfb ~dsfb ~roll ~lsb ~speed ~shb ~shs =
  { sfb = List.sum (module Float) (Hf.Table.data sfb) ~f:Fn.id
  ; dsfb = List.sum (module Float) (Hf.Table.data dsfb) ~f:Fn.id
  ; rolls =
      (let v = List.sum (module Roll) (Hf.Table.data roll) ~f:Fn.id in
       v.inward +. v.outward)
  ; lsb = List.sum (module Float) (Hand.Table.data lsb) ~f:Fn.id
  ; speed = List.sum (module Float) (Hf.Table.data speed) ~f:Fn.id
  ; shb = List.sum (module Float) (Hand.Table.data shb) ~f:Fn.id
  ; shs = List.sum (module Float) (Hand.Table.data shs) ~f:Fn.id
  }
;;

let incr : t Incr.t =
  map7
    Sfb.incr
    Dsfb.incr
    Roll.incr
    Lsb.incr
    Speed.incr
    Shb.incr
    Shs.incr
    ~f:(fun sfb dsfb roll lsb speed shb shs ->
      make ~sfb ~dsfb ~roll ~lsb ~speed ~shb ~shs)
;;
