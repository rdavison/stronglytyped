open! Import
open! Incr

type t = float

let make { Stats.sfb; weight } =
  let total_sfb = List.sum (module Float) (Hf.Table.data sfb) ~f:Fn.id in
  let _total_weight = List.sum (module Float) (Hf.Table.data weight) ~f:Fn.id in
  total_sfb
;;

let incr : t Incr.t = map Stats.incr ~f:make
