open! Import

let ( < ) = Float.( < )

let sfb =
  let%bind.Incr conf = Config.Incr.C.sfb in
  Imap.mapi Stats.sfb ~f:(fun ~key ~data ->
      let _, finger = key in
      let c, w = Finger.Map.find_exn conf finger in
      let wv = data *. w in
      if wv < c then 0. else Float.abs (c -. wv))
;;

let sfb_total = Imap.sum sfb (module Float) ~f:Fn.id

let dsfb =
  let%bind.Incr conf = Config.Incr.C.dsfb in
  Imap.mapi Stats.dsfb ~f:(fun ~key ~data ->
      let _, finger = key in
      let c, w = Finger.Map.find_exn conf finger in
      let wv = data *. w in
      if wv < c then 0. else Float.abs (c -. wv))
;;

let dsfb_total = Imap.sum dsfb (module Float) ~f:Fn.id

let speed =
  let%bind.Incr conf = Config.Incr.C.speed in
  Imap.mapi Stats.speed ~f:(fun ~key ~data ->
      let _, finger = key in
      let c, w = Finger.Map.find_exn conf finger in
      let wv = data *. w in
      Float.abs (c -. wv))
;;

let speed_total = Imap.sum speed (module Float) ~f:Fn.id

let lsb =
  let%bind.Incr c, w = Config.Incr.C.lsb in
  Imap.mapi Stats.lsb ~f:(fun ~key:_ ~data ->
      let wv = data *. w in
      if wv < c then 0. else Float.abs (c -. wv))
;;

let lsb_total = Imap.sum lsb (module Float) ~f:Fn.id

let roll =
  let%bind.Incr c, w_in, w_out = Config.Incr.C.roll in
  let w = (w_in +. w_out) /. 2. in
  let%map.Incr res =
    Imap.mapi Stats.roll ~f:(fun ~key:_ ~data ->
        let wv = 1. -. (w *. data) in
        if wv < c then 0. else Float.abs (c -. wv))
  in
  res
;;

let roll_total =
  let%map.Incr sum = Imap.sum roll (module Float) ~f:(fun i -> 1. -. i) in
  1. -. sum
;;

let dshrc =
  let%bind.Incr c, w = Incr.return (0., 2.) in
  let%map.Incr res =
    let dshrc =
      let%map_open.Incr dshrc_good = Stats.dshrc_good
      and dshrc_bad = Stats.dshrc_bad in
      List.fold Hand.all ~init:Hand.Map.empty ~f:(fun map h ->
          let good = Map.find_exn dshrc_good h in
          let bad = Map.find_exn dshrc_bad h in
          Map.add_exn map ~key:h ~data:(good, bad))
    in
    Imap.mapi dshrc ~f:(fun ~key:_ ~data ->
        let good, bad = data in
        let wv = good +. (w *. bad) in
        if wv < c then 0. else Float.abs (c -. wv))
  in
  res
;;

let dshrc_total = Imap.sum dshrc (module Float) ~f:Fn.id
let _incr = Incr.sum_float [| dshrc_total; roll_total; lsb_total; speed_total |]

let invert f =
  let%map.Incr f = f in
  1. -. f
;;

let w w f =
  let%map.Incr f = f in
  w *. f
;;

let incr =
  Incr.sum_float
    [| Stats.speed_total |> w 2.
     ; Stats.lsb_total |> w 3.
     ; Stats.dshrc_bad_total
     ; Stats.roll_total |> invert
    |]
;;
