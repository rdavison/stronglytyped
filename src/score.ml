open! Import
open! Incr

type t = float

let make { Stats.sfb; dsfb; weight } =
  let total_sfb = List.sum (module Float) (Hf.Table.data sfb) ~f:Fn.id in
  let total_dsfb = List.sum (module Float) (Hf.Table.data dsfb) ~f:Fn.id in
  let w_weight =
    Hf.Table.mapi weight ~f:(fun ~key:(_, f) ~data ->
        let mult =
          1.
          -.
          match f with
          | `P -> 1.5 /. 5.5
          | `R -> 3.6 /. 5.5
          | `M -> 4.8 /. 5.5
          | `I -> 5.5 /. 5.5
        in
        mult *. data)
  in
  let total_w_weight =
    List.sum (module Float) (Hf.Table.data w_weight) ~f:Fn.id
    /. Float.of_int (Hf.Table.length w_weight)
  in
  (0.25 *. total_sfb) +. (0.125 *. total_dsfb) +. ((1. -. 0.25 -. 0.125) *. total_w_weight)
;;

let incr : t Incr.t = map Stats.incr ~f:make
