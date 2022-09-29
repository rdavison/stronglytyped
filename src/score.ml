open! Import
open! Incr

type t = float

let make ~sfb ~dsfb ~roll ~lsb ~weight ~w_sfb ~w_dsfb ~w_weight ~w_rolls ~w_lsbs =
  let total_sfb = List.sum (module Float) (Hf.Table.data sfb) ~f:Fn.id in
  let total_dsfb = List.sum (module Float) (Hf.Table.data dsfb) ~f:Fn.id in
  let total_rolls = List.sum (module Roll) (Hf.Table.data roll) ~f:Fn.id in
  let total_w_weight =
    let inea =
      Hf.Table.mapi weight ~f:(fun ~key ~data ->
          let _, f = key in
          Finger.Table.find_exn w_weight f *. data)
    in
    List.sum (module Float) (Hf.Table.data inea) ~f:Fn.id
    /. Float.of_int (Finger.Table.length w_weight)
  in
  (w_sfb *. total_sfb)
  +. (w_dsfb *. total_dsfb)
  +. total_w_weight
  +. (w_rolls *. (1. -. total_rolls.inward))
  +. (w_rolls *. (1. -. total_rolls.outward))
  +. (w_lsbs *. List.sum (module Float) (Hand.Table.data lsb) ~f:Fn.id)
;;

let incr : t Incr.t =
  map10
    Sfb.incr
    Dsfb.incr
    Roll.incr
    Lsb.incr
    Weight.incr
    Config.w_sfb
    Config.w_dsfb
    Config.w_weight
    Config.w_rolls
    Config.w_lsbs
    ~f:(fun sfb dsfb roll lsb weight w_sfb w_dsfb w_weight w_rolls w_lsbs ->
      make ~sfb ~dsfb ~roll ~lsb ~weight ~w_sfb ~w_dsfb ~w_weight ~w_rolls ~w_lsbs)
;;
