open! Import
open! Incr

type t = float

let ( < ) = Float.( < )
(* let ( > ) = Float.( > ) *)

let sfb =
  let%bind.Incr conf = Config.Incr.C.sfb in
  Imap.mapi Sf.B.incr ~f:(fun ~key ~data ->
      let _, finger = key in
      let c, w = Finger.Map.find_exn conf finger in
      let wv = data *. w in
      if wv < c then 0. else Float.abs (c -. wv))
;;

let sfb_total = Imap.sum sfb (module Float) ~f:Fn.id

let dsfb =
  let%bind.Incr conf = Config.Incr.C.dsfb in
  Imap.mapi Sf.S.incr ~f:(fun ~key ~data ->
      let _, finger = key in
      let c, w = Finger.Map.find_exn conf finger in
      let wv = data *. w in
      if wv < c then 0. else Float.abs (c -. wv))
;;

let dsfb_total = Imap.sum dsfb (module Float) ~f:Fn.id

let speed =
  let%bind.Incr conf = Config.Incr.C.speed in
  Imap.mapi Sf.Speed.incr ~f:(fun ~key ~data ->
      let _, finger = key in
      let c, w = Finger.Map.find_exn conf finger in
      let wv = data *. w in
      if wv < c then 0. else Float.abs (c -. wv))
;;

let speed_total = Imap.sum speed (module Float) ~f:Fn.id

let lsb =
  let%bind.Incr c, w = Config.Incr.C.lsb in
  Imap.mapi Sf.Lsb.incr ~f:(fun ~key:_ ~data ->
      let wv = data *. w in
      if wv < c then 0. else Float.abs (c -. wv))
;;

let lsb_total = Imap.sum lsb (module Float) ~f:Fn.id

let roll =
  let%bind.Incr c, w_in, w_out = Config.Incr.C.roll in
  let%map.Incr res =
    Imap.mapi Sf.Roll.incr ~f:(fun ~key:_ ~data:{ inward; outward } ->
        let wv = 1. -. ((inward *. w_in) +. (outward *. w_out)) in
        let res = if wv < c then 0. else Float.abs (c -. wv) in
        res)
  in
  res
;;

let roll_total =
  let%map.Incr sum = Imap.sum roll (module Float) ~f:(fun i -> 1. -. i) in
  1. -. sum
;;

let incr : t Incr.t =
  [| sfb_total; dsfb_total; speed_total; lsb_total; roll_total |] |> Incr.sum_float
;;
