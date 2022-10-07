open! Import

module Internal = struct
  let same_finger ?(dist = false) data =
    let keyset x = Keyset.(x |> hf |> pairs |> dedup2 |> incr2) in
    let%bind_open.Incr data = data
    and stagger = Stagger.incr in
    let%map.Incr assoc =
      Incr.all
      @@ let%map.List hf = Hf.all in
         let%map.Incr keyset = keyset hf in
         ( hf
         , List.sum
             (module Float)
             keyset
             ~f:(fun (k1, k2) ->
               let dist = if dist then Key.dist k1 k2 ~stagger else 1. in
               let freq = Ngrams.freq2 (k1, k2) ~data in
               dist *. freq) )
    in
    Hf.Map.of_alist_exn assoc
  ;;

  (* Same Finger Bigram *)
  module Sfb = struct
    let incr = same_finger Corpus.bigrams
    let total = Imap.sum incr (module Float) ~f:Fn.id
  end

  (* Disjoint Same Finger Bigram *)
  module Dsfb = struct
    let incr = same_finger Corpus.skipgrams
    let total = Imap.sum incr (module Float) ~f:Fn.id
  end

  module Speed = struct
    let incr = same_finger Corpus.allgrams ~dist:true
    let total = Imap.sum incr (module Float) ~f:Fn.id
  end

  (* Lateral Stretch Bigram *)
  module Lsb = struct
    let incr =
      let keyset x =
        let cols =
          match x with
          | `L -> [ 2; 4 ]
          | `R -> [ 5; 7 ]
        in
        Keyset.(columns cols |> pairs |> incr2)
      in
      let%bind_open.Incr data = Corpus.bigrams
      and stagger = Stagger.incr in
      let%map.Incr assoc =
        Incr.all
        @@ let%map.List hand = Hand.all in
           let%map.Incr keyset = keyset hand in
           ( hand
           , List.sum
               (module Float)
               keyset
               ~f:(fun (k1, k2) ->
                 let dist = Key.dist k1 k2 ~stagger in
                 let freq = Ngrams.freq2 (k1, k2) ~data in
                 dist *. freq) )
      in
      Hand.Map.of_alist_exn assoc
    ;;

    let total = Imap.sum incr (module Float) ~f:Fn.id
  end

  module Keyfreq = struct
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

    let incr =
      make_incr1 ~select_keys:cols ~f:(fun k ->
          let%map.Incr data = Corpus.monograms in
          calculate_sf1 k ~data)
    ;;

    let total = Imap.sum incr (module Float) ~f:Fn.id
  end

  module Roll = struct
    type t =
      { inward : float
      ; outward : float
      }
    [@@deriving sexp]

    let incr =
      let keyset x = Keyset.(x |> hr |> pairs |> unique_fingers2 |> incr2) in
      let%bind.Incr data = Corpus.bigrams in
      let%map.Incr assoc =
        Incr.all
        @@ let%map.List hr = Hr.all in
           let%map.Incr keyset = keyset hr in
           ( hr
           , List.sum
               (module Float)
               keyset
               ~f:(fun (k1, k2) -> Ngrams.freq2 (k1, k2) ~data) )
      in
      Hr.Map.of_alist_exn assoc
    ;;

    let total = Imap.sum incr (module Float) ~f:Fn.id
    let () = ignore total
  end

  (* Disjoint Same Hand Row Change *)
  module Dshrc = struct
    module T : sig
      type t = float * float [@@deriving sexp, compare, hash]

      val good : float -> t
      val bad : float -> t

      include Container.Summable with type t := t
      include Abstract_algebra.Commutative_group.S with type t := t
    end = struct
      type t = float * float [@@deriving sexp, compare, hash]

      let ( + ) (a1, b1) (a2, b2) = a1 +. a2, b1 +. b2
      let ( - ) (a1, b1) (a2, b2) = a1 -. a2, b1 -. b2
      let zero = 0., 0.
      let good x = x, 0.
      let bad x = 0., x
    end

    include T

    let good_or_bad k1 k2 ~stagger =
      let slope = Float.compare (Key.slope k1 k2 ~stagger) 0. in
      if slope = 0
      then good
      else if slope < 0
      then (
        match k1.hand, k1.finger, k2.finger with
        | `L, `P, (`R | `M) -> bad
        | `L, `P, _ -> good
        | `L, `R, (`P | `M) -> bad
        | `L, `R, _ -> good
        | `L, `M, (`P | `R) -> bad
        | `L, `M, _ -> good
        | `L, `I, _ -> good
        | `R, `I, _ -> bad
        | `R, `M, (`P | `R) -> good
        | `R, `M, _ -> bad
        | `R, `R, (`P | `M) -> good
        | `R, `R, _ -> bad
        | `R, `P, (`R | `M) -> good
        | `R, `P, _ -> bad)
      else (
        match k1.hand, k1.finger, k2.finger with
        | `L, `P, (`R | `M) -> good
        | `L, `P, _ -> bad
        | `L, `R, (`P | `M) -> good
        | `L, `R, _ -> bad
        | `L, `M, (`P | `R) -> good
        | `L, `M, _ -> bad
        | `L, `I, _ -> bad
        | `R, `I, _ -> good
        | `R, `M, (`P | `R) -> bad
        | `R, `M, _ -> good
        | `R, `R, (`P | `M) -> bad
        | `R, `R, _ -> good
        | `R, `P, (`R | `M) -> bad
        | `R, `P, _ -> good)
    ;;

    let calc data =
      let keyset x =
        Keyset.(x |> hand |> pairs |> dedup2 |> unique_fingers2 |> symmetric2 |> incr2)
      in
      let%bind_open.Incr data = data
      and stagger = Stagger.incr in
      let%map.Incr assoc =
        let l =
          let%map.List hand = Hand.all in
          let%map.Incr keyset = keyset hand in
          let sum =
            List.sum
              (module T)
              keyset
              ~f:(fun (k1, k2) ->
                let freq = Ngrams.freq2 (k1, k2) ~data in
                let dist = Key.dist k1 k2 ~stagger in
                let freq_per_dist = freq /. dist in
                let k12 = good_or_bad k1 k2 in
                let k21 = good_or_bad k2 k1 in
                T.(k12 freq_per_dist ~stagger + k21 freq_per_dist ~stagger))
          in
          hand, sum
        in
        Incr.all l
      in
      Hand.Map.of_alist_exn assoc
    ;;

    let incr = calc Corpus.bigrams
    let total = Imap.sum incr (module T) ~f:Fn.id
    let () = ignore total
  end
end

open Internal

let sfb = Sfb.incr
let sfb_total = Sfb.total
let dsfb = Dsfb.incr
let dsfb_total = Dsfb.total
let speed = Speed.incr
let speed_total = Speed.total
let lsb = Lsb.incr
let lsb_total = Lsb.total
let keyfreq = Keyfreq.incr
let keyfreq_total = Keyfreq.total
let hr_roll = Incr.return (Hr.Map.of_alist_exn (Hr.all |> List.map ~f:(fun hr -> hr, 0.)))

let hr_roll_in =
  Incr.return (Hr.Map.of_alist_exn (Hr.all |> List.map ~f:(fun hr -> hr, 0.)))
;;

let hr_roll_out =
  Incr.return (Hr.Map.of_alist_exn (Hr.all |> List.map ~f:(fun hr -> hr, 0.)))
;;

let hr_roll_total = Incr.return 0.
let hr_roll_in_total = Incr.return 0.
let hr_roll_out_total = Incr.return 0.

let hand_roll =
  Incr.return (Hand.Map.of_alist_exn (Hand.all |> List.map ~f:(fun hand -> hand, 0.)))
;;

let hand_roll_in =
  Incr.return (Hand.Map.of_alist_exn (Hand.all |> List.map ~f:(fun hand -> hand, 0.)))
;;

let hand_roll_out =
  Incr.return (Hand.Map.of_alist_exn (Hand.all |> List.map ~f:(fun hand -> hand, 0.)))
;;

let hand_roll_total = Incr.return 0.
let hand_roll_in_total = Incr.return 0.
let hand_roll_out_total = Incr.return 0.

let dshrc =
  Incr.return (Hand.Map.of_alist_exn (Hand.all |> List.map ~f:(fun hand -> hand, 0.)))
;;

let dshrc_good =
  Incr.return (Hand.Map.of_alist_exn (Hand.all |> List.map ~f:(fun hand -> hand, 0.)))
;;

let dshrc_bad =
  Incr.return (Hand.Map.of_alist_exn (Hand.all |> List.map ~f:(fun hand -> hand, 0.)))
;;

let dshrc_total = Incr.return 0.
let dshrc_good_total = Incr.return 0.
let dshrc_bad_total = Incr.return 0.

type t =
  { sfb : float Hf.Map.t
  ; sfb_total : float
  ; dsfb : float Hf.Map.t
  ; dsfb_total : float
  ; speed : float Hf.Map.t
  ; speed_total : float
  ; lsb : float Hand.Map.t
  ; lsb_total : float
  ; keyfreq : float Hf.Map.t
  ; keyfreq_total : float
  ; hr_roll : float Hr.Map.t
  ; hr_roll_in : float Hr.Map.t
  ; hr_roll_out : float Hr.Map.t
  ; hr_roll_total : float
  ; hr_roll_in_total : float
  ; hr_roll_out_total : float
  ; hand_roll : float Hand.Map.t
  ; hand_roll_in : float Hand.Map.t
  ; hand_roll_out : float Hand.Map.t
  ; hand_roll_total : float
  ; hand_roll_in_total : float
  ; hand_roll_out_total : float
  ; dshrc : float Hand.Map.t
  ; dshrc_good : float Hand.Map.t
  ; dshrc_bad : float Hand.Map.t
  ; dshrc_total : float
  ; dshrc_good_total : float
  ; dshrc_bad_total : float
  }
[@@deriving sexp]

let to_string
    { sfb
    ; sfb_total
    ; dsfb
    ; dsfb_total
    ; speed
    ; speed_total
    ; lsb
    ; lsb_total
    ; keyfreq
    ; keyfreq_total
    ; hr_roll
    ; hr_roll_in = _
    ; hr_roll_out = _
    ; hr_roll_total = _
    ; hr_roll_in_total = _
    ; hr_roll_out_total = _
    ; hand_roll
    ; hand_roll_in
    ; hand_roll_out
    ; hand_roll_total
    ; hand_roll_in_total
    ; hand_roll_out_total
    ; dshrc
    ; dshrc_good
    ; dshrc_bad
    ; dshrc_total
    ; dshrc_good_total
    ; dshrc_bad_total
    }
  =
  let module T = Text_block in
  let table data ~to_string ~all =
    let cols =
      all
      |> List.map ~f:(fun hf ->
             let data =
               data
               |> List.map ~f:snd
               |> List.map ~f:(fun map ->
                      Map.find_exn (fst map) hf
                      |> Float.( * ) 100.
                      |> sprintf "%.2f"
                      |> T.text)
             in
             T.text (to_string hf) :: data, `Right)
      |> List.cons
           ( T.text "(total)"
             :: List.map data ~f:(fun x ->
                    x |> snd |> snd |> Float.( * ) 100. |> sprintf "%.2f" |> T.text)
           , `Right )
      |> List.cons (T.nil :: List.map data ~f:(Fn.compose T.text fst), `Right)
    in
    let (`Rows rows) = T.table (`Cols cols) in
    T.vcat rows
  in
  let hf_table =
    table
      ~all:Hf.all
      ~to_string:Hf.to_string
      [ "sfb", (sfb, sfb_total)
      ; "dsfb", (dsfb, dsfb_total)
      ; "speed", (speed, speed_total)
      ; "weight", (keyfreq, keyfreq_total)
      ]
  in
  let hand_table =
    table
      ~all:Hand.all
      ~to_string:Hand.to_string
      [ "lsb", (lsb, lsb_total)
      ; "dshrc", (dshrc, dshrc_total)
      ; "(good) dshrc", (dshrc_good, dshrc_good_total)
      ; "(bad) dshrc", (dshrc_bad, dshrc_bad_total)
      ; "roll", (hand_roll, hand_roll_total)
      ; "(in) roll", (hand_roll_in, hand_roll_in_total)
      ; "(out) roll", (hand_roll_out, hand_roll_out_total)
      ]
  in
  let hr_table =
    let data =
      List.map [ 0; 1; 2 ] ~f:(fun r ->
          let title =
            match r with
            | 0 -> "top"
            | 1 -> "middle"
            | 2 -> "bottom"
            | _ -> assert false
          in
          let lr =
            Hand.Map.of_alist_exn
              (List.map Hand.all ~f:(fun h -> h, Map.find_exn hr_roll (h, r)))
          in
          let total = Map.data lr |> List.sum (module Float) ~f:Fn.id in
          title, (lr, total))
    in
    table ~all:Hand.all ~to_string:Hand.to_string data
  in
  let sections = [ "hand-finger", hf_table; "hand", hand_table; "hand-row", hr_table ] in
  let t =
    sections
    |> List.map ~f:(fun (title, table) ->
           let title = title |> T.text in
           let table = table |> T.Boxed.cell |> T.boxed in
           T.vcat [ title; table ])
    |> T.vcat
  in
  T.render t
;;

let incr =
  let%map_open.Incr sfb = sfb
  and sfb_total = sfb_total
  and dsfb = dsfb
  and dsfb_total = dsfb_total
  and speed = speed
  and speed_total = speed_total
  and lsb = lsb
  and lsb_total = lsb_total
  and keyfreq = keyfreq
  and keyfreq_total = keyfreq_total
  and hr_roll = hr_roll
  and hr_roll_in = hr_roll_in
  and hr_roll_out = hr_roll_out
  and hr_roll_total = hr_roll_total
  and hr_roll_in_total = hr_roll_in_total
  and hr_roll_out_total = hr_roll_out_total
  and hand_roll = hand_roll
  and hand_roll_in = hand_roll_in
  and hand_roll_out = hand_roll_out
  and hand_roll_total = hand_roll_total
  and hand_roll_in_total = hand_roll_in_total
  and hand_roll_out_total = hand_roll_out_total
  and dshrc = dshrc
  and dshrc_good = dshrc_good
  and dshrc_bad = dshrc_bad
  and dshrc_total = dshrc_total
  and dshrc_good_total = dshrc_good_total
  and dshrc_bad_total = dshrc_bad_total in
  { sfb
  ; sfb_total
  ; dsfb
  ; dsfb_total
  ; speed
  ; speed_total
  ; lsb
  ; lsb_total
  ; keyfreq
  ; keyfreq_total
  ; hr_roll
  ; hr_roll_in
  ; hr_roll_out
  ; hr_roll_total
  ; hr_roll_in_total
  ; hr_roll_out_total
  ; hand_roll
  ; hand_roll_in
  ; hand_roll_out
  ; hand_roll_total
  ; hand_roll_in_total
  ; hand_roll_out_total
  ; dshrc
  ; dshrc_good
  ; dshrc_bad
  ; dshrc_total
  ; dshrc_good_total
  ; dshrc_bad_total
  }
;;
