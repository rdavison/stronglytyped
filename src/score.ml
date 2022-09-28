open! Import
open! Incr

type t = float

let make { Stats.sfb; weight } =
  let _ =
    let total_sfb = List.sum (module Float) (Hf.Table.data sfb) ~f:Fn.id in
    let total_weight = List.sum (module Float) (Hf.Table.data weight) ~f:Fn.id in
    total_sfb +. total_weight
  in
  Random.float 1.
;;

let incr : t Incr.t = map Stats.incr ~f:make
