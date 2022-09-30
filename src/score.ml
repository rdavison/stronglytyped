open! Import
open! Incr

type t = float

let make ~(totals : Totals.t) ~c_sfb ~c_dsfb ~c_roll ~c_lsb ~c_speed ~c_shb ~c_shs =
  let ( < ) = Float.( < ) in
  let ( > ) = Float.( > ) in
  let d_sfb = if totals.sfb < c_sfb then 0. else Float.abs (totals.sfb -. c_sfb) in
  let _d_dsfb = if totals.dsfb < c_dsfb then 0. else Float.abs (totals.dsfb -. c_dsfb) in
  let _d_rolls =
    if totals.rolls > c_roll then 0. else Float.abs (totals.rolls -. c_roll)
  in
  let _d_lsb = if totals.lsb < c_lsb then 0. else Float.abs (totals.lsb -. c_lsb) in
  let _d_shb = if totals.shb > c_shb then 0. else Float.abs (totals.shb -. c_shb) in
  let _d_shs = if totals.shs < c_shs then 0. else Float.abs (totals.shs -. c_shs) in
  let _d_speed =
    if totals.speed < c_speed then 0. else Float.abs (totals.speed -. c_speed)
  in
  d_sfb
;;

let incr : t Incr.t =
  map8
    Totals.incr
    Config.Incr.C.sfb
    Config.Incr.C.dsfb
    Config.Incr.C.roll
    Config.Incr.C.lsb
    Config.Incr.C.speed
    Config.Incr.C.shb
    Config.Incr.C.shs
    ~f:(fun totals c_sfb c_dsfb c_roll c_lsb c_speed c_shb c_shs ->
      make ~totals ~c_sfb ~c_dsfb ~c_roll ~c_lsb ~c_speed ~c_shb ~c_shs)
;;
