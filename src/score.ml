open! Import
open! Incr

type t = float

let make ~sfb ~dsfb ~roll ~lsb ~c_sfb ~c_dsfb ~c_roll ~c_lsb =
  let ( < ) = Float.( < ) in
  let ( > ) = Float.( > ) in
  let total_sfb = List.sum (module Float) (Hf.Table.data sfb) ~f:Fn.id in
  let total_dsfb = List.sum (module Float) (Hf.Table.data dsfb) ~f:Fn.id in
  let total_rolls = List.sum (module Roll) (Hf.Table.data roll) ~f:Fn.id in
  let total_rolls = total_rolls.inward +. total_rolls.outward in
  let total_lsb = List.sum (module Float) (Hand.Table.data lsb) ~f:Fn.id in
  let d_sfb = if total_sfb < c_sfb then 0. else Float.abs (total_sfb -. c_sfb) in
  let d_dsfb = if total_dsfb < c_dsfb then 0. else Float.abs (total_dsfb -. c_dsfb) in
  let d_rolls = if total_rolls > c_roll then 0. else Float.abs (total_rolls -. c_roll) in
  let d_lsb = if total_lsb < c_lsb then 0. else Float.abs (total_lsb -. c_lsb) in
  d_sfb +. d_dsfb +. d_rolls +. d_lsb
;;

let incr : t Incr.t =
  map8
    Sfb.incr
    Dsfb.incr
    Roll.incr
    Lsb.incr
    Config.c_sfb
    Config.c_dsfb
    Config.w_roll
    Config.w_lsb
    ~f:(fun sfb dsfb roll lsb c_sfb c_dsfb c_roll c_lsb ->
      make ~sfb ~dsfb ~roll ~lsb ~c_sfb ~c_dsfb ~c_roll ~c_lsb)
;;
