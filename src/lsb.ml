open! Import
open! Incr

type t = float

let incr : t Hand.Table.t Incr.t =
  let open Incr.Let_syntax in
  let%bind bigrams = Config.bigrams in
  let get = Hf.Table.find_exn By_hf.table in
  let lm = get (`L, `M) in
  let li = get (`L, `I) in
  let rm = get (`R, `M) in
  let ri = get (`R, `I) in
  let left = map2 lm li ~f:(fun a b -> a, b) in
  let right = map2 rm ri ~f:(fun a b -> a, b) in
  let%map (lm, li), (rm, ri) = Incr.both left right in
  let hand (m, i) =
    List.fold m ~init:0. ~f:(fun init k1 ->
        List.fold i ~init ~f:(fun acc k2 ->
            let c1, c2 = k1.Key.code, k2.Key.code in
            acc
            +.
            match c1, c2 with
            | `Char c1, `Char c2 ->
              let f12 =
                String.Table.find_or_add
                  bigrams
                  (String.of_char_list [ c1; c2 ])
                  ~default:(Fn.const 0.)
              in
              let f21 =
                String.Table.find_or_add
                  bigrams
                  (String.of_char_list [ c2; c1 ])
                  ~default:(Fn.const 0.)
              in
              f12 +. f21))
  in
  let left = hand (lm, li) in
  let right = hand (rm, ri) in
  Hand.Table.of_alist_exn [ `L, left; `R, right ]
;;
