open! Import
open! Incr

type t = float [@@deriving sexp, compare, hash]

let make keys ~bigrams =
  List.fold keys ~init:0. ~f:(fun init k1 ->
      List.fold keys ~init ~f:(fun acc k2 ->
          match Key.equal k1 k2 with
          | true -> acc
          | false ->
            acc
            +.
            let c1, c2 = k1.Key.code, k2.Key.code in
            (match c1, c2 with
            | `Char c1, `Char c2 ->
              String.Table.find_or_add
                bigrams
                (String.of_char_list [ c1; c2 ])
                ~default:(fun () -> 0.))))
;;

(* let incr =
  let%bind.Incr bigrams = Corpus.bigrams in
  By_hf.table
  |> Hf.Table.map ~f:(map ~f:(make ~bigrams))
  |> Hf.Table.to_alist
  |> List.map ~f:(fun (key, data) -> map data ~f:(fun data -> key, data))
  |> all
  |> map ~f:Hf.Table.of_alist_exn
;; *)

(* let incr2 =
  let%bind.Incr bigrams = Corpus.bigrams in
  Imap.map By_hf.table ~f:(make ~bigrams)
;; *)

module M = struct
  module T = struct
    type nonrec t = Hf.t * t [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

let make_incr ~f ~select_keys =
  let ks cols =
    let keys =
      let%bind.List c = cols in
      [ Key.all_arr_incr.(index 0 c)
      ; Key.all_arr_incr.(index 1 c)
      ; Key.all_arr_incr.(index 2 c)
      ]
    in
    Array.of_list
    @@ let%map_open.List k1 = keys
       and k2 = keys in
       let%bind_open.Incr k1 = k1
       and k2 = k2 in
       f k1 k2
  in
  let l =
    let%map.List hf = Hf.all in
    let%map.Incr data = select_keys hf |> ks |> Incr.sum_float in
    hf, data
  in
  let%map.Incr l = Incr.all l in
  Hf.Map.of_alist_exn l
;;

let make_incr1 ~f ~select_keys =
  let ks cols =
    let keys =
      let%bind.List c = cols in
      [ Key.all_arr_incr.(index 0 c)
      ; Key.all_arr_incr.(index 1 c)
      ; Key.all_arr_incr.(index 2 c)
      ]
    in
    Array.of_list
    @@ let%map.List k = keys in
       let%bind.Incr k = k in
       f k
  in
  let l =
    let%map.List hf = Hf.all in
    let%map.Incr data = select_keys hf |> ks |> Incr.sum_float in
    hf, data
  in
  let%map.Incr l = Incr.all l in
  Hf.Map.of_alist_exn l
;;

let make_total incr = Imap.sum incr (module Float) ~f:Fn.id

let calculate_sf ?(dist = 1.) k1 k2 ~data =
  if [%compare.equal: Key.t] k1 k2
  then 0.
  else (
    let c1, c2 = k1.Key.code, k2.Key.code in
    match c1, c2 with
    | `Char c1, `Char c2 ->
      let freq =
        String.Table.find_or_add
          data
          (String.of_char_list [ c1; c2 ])
          ~default:(fun () -> 0.)
      in
      freq *. dist)
;;

let calculate_sf1 ?(dist = 1.) k ~data =
  let c = k.Key.code in
  match c with
  | `Char c ->
    let freq = Char.Table.find_or_add data c ~default:(fun () -> 0.) in
    freq *. dist
;;

let cols = function
  | `L, `P -> [ 0 ]
  | `L, `R -> [ 1 ]
  | `L, `M -> [ 2 ]
  | `L, `I -> [ 3; 4 ]
  | `R, `I -> [ 5; 6 ]
  | `R, `M -> [ 7 ]
  | `R, `R -> [ 8 ]
  | `R, `P -> [ 9 ]
;;

module B = struct
  let incr =
    make_incr ~select_keys:cols ~f:(fun k1 k2 ->
        let%map.Incr data = Corpus.bigrams in
        calculate_sf k1 k2 ~data)
  ;;

  let total = make_total incr
end

module S = struct
  let incr =
    make_incr ~select_keys:cols ~f:(fun k1 k2 ->
        let%map.Incr data = Corpus.skipgrams in
        calculate_sf k1 k2 ~data)
  ;;

  let total = make_total incr
end

module Speed = struct
  let incr =
    make_incr ~select_keys:cols ~f:(fun k1 k2 ->
        let%map.Incr data = Corpus.allgrams in
        let dist = Key.dist k1 k2 in
        calculate_sf k1 k2 ~data ~dist)
  ;;

  let total = make_total incr
end

module Lsb = struct
  let left_hand = function
    | `L, `P -> []
    | `L, `R -> []
    | `L, `M -> [ 2 ]
    | `L, `I -> [ 4 ]
    | `R, `I -> []
    | `R, `M -> []
    | `R, `R -> []
    | `R, `P -> []
  ;;

  let right_hand = function
    | `L, `P -> []
    | `L, `R -> []
    | `L, `M -> []
    | `L, `I -> []
    | `R, `I -> [ 5 ]
    | `R, `M -> [ 7 ]
    | `R, `R -> []
    | `R, `P -> []
  ;;

  let left =
    let map =
      make_incr ~select_keys:left_hand ~f:(fun k1 k2 ->
          let%map.Incr data = Corpus.bigrams in
          calculate_sf k1 k2 ~data)
    in
    Imap.sum map (module Float) ~f:Fn.id
  ;;

  let right =
    let map =
      make_incr ~select_keys:right_hand ~f:(fun k1 k2 ->
          let%map.Incr data = Corpus.bigrams in
          calculate_sf k1 k2 ~data)
    in
    Imap.sum map (module Float) ~f:Fn.id
  ;;

  let incr =
    Incr.map2 left right ~f:(fun left right ->
        Hand.Map.of_alist_exn [ `L, left; `R, right ])
  ;;

  let total = make_total incr
end

module Keyfreq = struct
  let incr =
    make_incr1 ~select_keys:cols ~f:(fun k ->
        let%map.Incr data = Corpus.monograms in
        calculate_sf1 k ~data)
  ;;

  let total = make_total incr
end

module Roll = struct
  type t =
    { inward : float
    ; outward : float
    }
  [@@deriving sexp]

  let zero = { inward = 0.; outward = 0. }
  let add a b = { inward = a.inward +. b.inward; outward = a.outward +. b.outward }
  let sub a b = { inward = a.inward -. b.inward; outward = a.outward -. b.outward }

  let select_keys = function
    | `L -> [ 0; 1; 2; 3; 4 ]
    | `R -> [ 9; 8; 7; 6; 5 ]
  ;;

  let make_incr ~f =
    let ks r cols =
      let keys = List.map cols ~f:(fun c -> Key.all_arr_incr.(index r c)) in
      Array.of_list
      @@ let%map_open.List k1 = keys
         and k2 = keys in
         let%bind_open.Incr k1 = k1
         and k2 = k2 in
         f k1 k2
    in
    let l =
      let%map.List ((h, r) as hr) = Hr.all in
      let%map.Incr data = select_keys h |> ks r |> Incr.sum ~zero ~add ~sub in
      hr, data
    in
    let%map.Incr l = Incr.all l in
    Hr.Map.of_alist_exn l
  ;;

  let direction (k1 : Key.t) (k2 : Key.t) =
    let ( - ) = Int.( - ) in
    let f1 = Finger.to_int k1.finger in
    let f2 = Finger.to_int k2.finger in
    match k1.hand with
    | `L -> if f2 - f1 > 0 then `O else `I
    | `R -> if f2 - f2 > 0 then `I else `O
  ;;

  let calc (k1 : Key.t) (k2 : Key.t) =
    let%map.Incr bigrams = Corpus.bigrams in
    match
      [%compare.equal: Hand.t] k1.hand k2.hand
      && not ([%compare.equal: Finger.t] k1.finger k2.finger)
    with
    | false -> zero
    | true ->
      let c1, c2 = k1.Key.code, k2.Key.code in
      (match c1, c2 with
      | `Char c1, `Char c2 ->
        let freq =
          String.Table.find_or_add
            bigrams
            (String.of_char_list [ c1; c2 ])
            ~default:(fun () -> 0.)
        in
        (match direction k1 k2 with
        | `I -> { inward = freq; outward = 0. }
        | `O -> { inward = 0.; outward = freq }))
  ;;

  let incr =
    make_incr ~f:(fun k1 k2 ->
        let%map.Incr res = calc k1 k2 in
        res)
  ;;

  let total =
    Imap.sum
      incr
      (module struct
        type nonrec t = t

        let ( + ) = add
        let ( - ) = sub
        let zero = zero
      end)
      ~f:Fn.id
  ;;
end
