open! Import

module Internal = struct
  module Sf = struct
    let keyset x = Keyset.(x |> hf |> pairs |> no_repeats2 |> incr2)

    let make ?(dist = false) data =
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
  end

  (* Same Finger Bigram *)
  module Sfb = struct
    let keyset = Sf.keyset
    let incr = Sf.make Corpus.bigrams
    let total = Imap.sum incr (module Float) ~f:Fn.id
  end

  (* Disjoint Same Finger Bigram *)
  module Dsfb = struct
    let keyset = Sf.keyset
    let incr = Sf.make Corpus.skipgrams
    let total = Imap.sum incr (module Float) ~f:Fn.id
  end

  module Speed = struct
    let keyset = Sf.keyset
    let incr = Sf.make Corpus.allgrams ~dist:true
    let total = Imap.sum incr (module Float) ~f:Fn.id
  end

  (* Lateral Stretch Bigram *)
  module Lsb = struct
    let keyset x =
      let cols =
        match x with
        | `L -> [ 2; 4 ]
        | `R -> [ 5; 7 ]
      in
      Keyset.(columns cols |> pairs |> no_repeats2 |> unique_fingers2 |> incr2)
    ;;

    let incr =
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

  module Float2 : sig
    type t = float * float [@@deriving sexp, compare, hash]

    val combine : t -> float

    include Container.Summable with type t := t
    include Abstract_algebra.Commutative_group.S with type t := t
  end = struct
    type t = float * float [@@deriving sexp, compare, hash]

    let combine (x, y) = x +. y
    let ( + ) (a1, b1) (a2, b2) = a1 +. a2, b1 +. b2
    let ( - ) (a1, b1) (a2, b2) = a1 -. a2, b1 -. b2
    let zero = 0., 0.
  end

  module Roll = struct
    let keyset x = Keyset.(x |> hr |> pairs |> unique_fingers2 |> incr2)

    let make_incr row =
      let%bind.Incr data = Corpus.bigrams in
      let%map.Incr assoc =
        Incr.all
        @@ let%map.List hand = Hand.all in
           let%map.Incr keyset = keyset (hand, row) in
           ( hand
           , List.sum
               (module Float2)
               keyset
               ~f:(fun (k1, k2) ->
                 let freq = Ngrams.freq2 (k1, k2) ~data in
                 let dir = Key.direction k1 k2 in
                 match dir with
                 | `I -> freq, 0.
                 | `O -> 0., freq) )
      in
      Hand.Map.of_alist_exn assoc
    ;;

    let make_total2 incr = Imap.sum incr (module Float) ~f:Float2.combine
    let make_total incr = Imap.sum incr (module Float) ~f:Fn.id
    let roll_top2 = make_incr 0
    let roll_top_total = make_total2 roll_top2
    let roll_in_top = Imap.map roll_top2 ~f:fst
    let roll_in_top_total = make_total roll_in_top
    let roll_out_top = Imap.map roll_top2 ~f:snd
    let roll_out_top_total = make_total roll_out_top
    let roll_middle2 = make_incr 1
    let roll_middle_total = make_total2 roll_middle2
    let roll_in_middle = Imap.map roll_middle2 ~f:fst
    let roll_in_middle_total = make_total roll_in_middle
    let roll_out_middle = Imap.map roll_middle2 ~f:snd
    let roll_out_middle_total = make_total roll_out_middle
    let roll_bottom2 = make_incr 2
    let roll_bottom_total = make_total2 roll_bottom2
    let roll_in_bottom = Imap.map roll_bottom2 ~f:fst
    let roll_in_bottom_total = make_total roll_in_bottom
    let roll_out_bottom = Imap.map roll_bottom2 ~f:snd
    let roll_out_bottom_total = make_total roll_out_bottom

    let merge a b =
      Imap.merge a b ~f:(fun ~key:_ merge_elem ->
          let left, right =
            Map.Merge_element.values
              merge_elem
              ~left_default:(0., 0.)
              ~right_default:(0., 0.)
          in
          Some (Float2.( + ) left right))
    ;;

    let roll2 = merge roll_top2 roll_middle2 |> merge roll_bottom2
    let roll = Imap.map roll2 ~f:Float2.combine
    let roll_top = Imap.map roll_top2 ~f:Float2.combine
    let roll_middle = Imap.map roll_middle2 ~f:Float2.combine
    let roll_bottom = Imap.map roll_bottom2 ~f:Float2.combine
    let roll_total = make_total2 roll2
    let roll_in = Imap.map roll2 ~f:fst
    let roll_in_total = make_total roll_in
    let roll_out = Imap.map roll2 ~f:snd
    let roll_out_total = make_total roll_out
  end

  (* Disjoint Same Hand Row Change *)
  module Dshrc = struct
    let good_or_bad k1 k2 ~stagger =
      let slope = Float.compare (Key.slope k1 k2 ~stagger) 0. in
      let good x = x, 0. in
      let bad x = 0., x in
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

    let keyset x =
      Keyset.(x |> hand |> pairs |> dedup2 |> unique_fingers2 |> symmetric2 |> incr2)
    ;;

    let calc data =
      let%bind_open.Incr data = data
      and stagger = Stagger.incr in
      let%map.Incr assoc =
        let l =
          let%map.List hand = Hand.all in
          let%map.Incr keyset = keyset hand in
          let sum =
            List.sum
              (module Float2)
              keyset
              ~f:(fun (k1, k2) ->
                let freq = Ngrams.freq2 (k1, k2) ~data in
                let dist = Key.dist k1 k2 ~stagger in
                let freq_per_dist = freq /. dist in
                let k12 = good_or_bad k1 k2 in
                let k21 = good_or_bad k2 k1 in
                Float2.(k12 freq_per_dist ~stagger + k21 freq_per_dist ~stagger))
          in
          hand, sum
        in
        Incr.all l
      in
      Hand.Map.of_alist_exn assoc
    ;;

    let make_total incr = Imap.sum incr (module Float) ~f:Fn.id
    let dshrc2 = calc Corpus.bigrams
    let dshrc = Imap.map dshrc2 ~f:Float2.combine
    let dshrc_total = make_total dshrc
    let dshrc_good = Imap.map dshrc2 ~f:fst
    let dshrc_good_total = make_total dshrc_good
    let dshrc_bad = Imap.map dshrc2 ~f:snd
    let dshrc_bad_total = make_total dshrc_bad
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
let roll = Roll.roll
let roll_total = Roll.roll_total
let roll_top = Roll.roll_top
let roll_top_total = Roll.roll_top_total
let roll_middle = Roll.roll_middle
let roll_middle_total = Roll.roll_middle_total
let roll_bottom = Roll.roll_bottom
let roll_bottom_total = Roll.roll_bottom_total
let roll_in = Roll.roll_in
let roll_in_total = Roll.roll_in_total
let roll_in_top = Roll.roll_in_top
let roll_in_top_total = Roll.roll_in_top_total
let roll_in_middle = Roll.roll_in_middle
let roll_in_middle_total = Roll.roll_in_middle_total
let roll_in_bottom = Roll.roll_in_bottom
let roll_in_bottom_total = Roll.roll_in_bottom_total
let roll_out = Roll.roll_out
let roll_out_total = Roll.roll_out_total
let roll_out_top = Roll.roll_out_top
let roll_out_top_total = Roll.roll_out_top_total
let roll_out_middle = Roll.roll_out_middle
let roll_out_middle_total = Roll.roll_out_middle_total
let roll_out_bottom = Roll.roll_out_bottom
let roll_out_bottom_total = Roll.roll_out_bottom_total
let dshrc = Dshrc.dshrc
let dshrc_total = Dshrc.dshrc_total
let dshrc_good = Dshrc.dshrc_good
let dshrc_good_total = Dshrc.dshrc_good_total
let dshrc_bad = Dshrc.dshrc_bad
let dshrc_bad_total = Dshrc.dshrc_bad_total

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
  ; roll : float Hand.Map.t
  ; roll_total : float
  ; roll_top : float Hand.Map.t
  ; roll_top_total : float
  ; roll_middle : float Hand.Map.t
  ; roll_middle_total : float
  ; roll_bottom : float Hand.Map.t
  ; roll_bottom_total : float
  ; roll_in : float Hand.Map.t
  ; roll_in_total : float
  ; roll_in_top : float Hand.Map.t
  ; roll_in_top_total : float
  ; roll_in_middle : float Hand.Map.t
  ; roll_in_middle_total : float
  ; roll_in_bottom : float Hand.Map.t
  ; roll_in_bottom_total : float
  ; roll_out : float Hand.Map.t
  ; roll_out_total : float
  ; roll_out_top : float Hand.Map.t
  ; roll_out_top_total : float
  ; roll_out_middle : float Hand.Map.t
  ; roll_out_middle_total : float
  ; roll_out_bottom : float Hand.Map.t
  ; roll_out_bottom_total : float
  ; dshrc : float Hand.Map.t
  ; dshrc_total : float
  ; dshrc_good : float Hand.Map.t
  ; dshrc_good_total : float
  ; dshrc_bad : float Hand.Map.t
  ; dshrc_bad_total : float
  }
[@@deriving sexp, compare]

let empty =
  { sfb = Hf.Map.empty
  ; sfb_total = 0.
  ; dsfb = Hf.Map.empty
  ; dsfb_total = 0.
  ; speed = Hf.Map.empty
  ; speed_total = 0.
  ; lsb = Hand.Map.empty
  ; lsb_total = 0.
  ; keyfreq = Hf.Map.empty
  ; keyfreq_total = 0.
  ; roll = Hand.Map.empty
  ; roll_total = 0.
  ; roll_top = Hand.Map.empty
  ; roll_top_total = 0.
  ; roll_middle = Hand.Map.empty
  ; roll_middle_total = 0.
  ; roll_bottom = Hand.Map.empty
  ; roll_bottom_total = 0.
  ; roll_in = Hand.Map.empty
  ; roll_in_total = 0.
  ; roll_in_top = Hand.Map.empty
  ; roll_in_top_total = 0.
  ; roll_in_middle = Hand.Map.empty
  ; roll_in_middle_total = 0.
  ; roll_in_bottom = Hand.Map.empty
  ; roll_in_bottom_total = 0.
  ; roll_out = Hand.Map.empty
  ; roll_out_total = 0.
  ; roll_out_top = Hand.Map.empty
  ; roll_out_top_total = 0.
  ; roll_out_middle = Hand.Map.empty
  ; roll_out_middle_total = 0.
  ; roll_out_bottom = Hand.Map.empty
  ; roll_out_bottom_total = 0.
  ; dshrc = Hand.Map.empty
  ; dshrc_total = 0.
  ; dshrc_good = Hand.Map.empty
  ; dshrc_good_total = 0.
  ; dshrc_bad = Hand.Map.empty
  ; dshrc_bad_total = 0.
  }
;;

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
    ; roll
    ; roll_total
    ; roll_top
    ; roll_top_total
    ; roll_middle
    ; roll_middle_total
    ; roll_bottom
    ; roll_bottom_total
    ; roll_in
    ; roll_in_total
    ; roll_in_top
    ; roll_in_top_total
    ; roll_in_middle
    ; roll_in_middle_total
    ; roll_in_bottom
    ; roll_in_bottom_total
    ; roll_out
    ; roll_out_total
    ; roll_out_top
    ; roll_out_top_total
    ; roll_out_middle
    ; roll_out_middle_total
    ; roll_out_bottom
    ; roll_out_bottom_total
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
      ; "roll", (roll, roll_total)
      ; " (in) roll", (roll_in, roll_in_total)
      ; "(out) roll", (roll_out, roll_out_total)
      ; "(top)       roll", (roll_top, roll_top_total)
      ; "(middle)       roll", (roll_middle, roll_middle_total)
      ; "(bottom)       roll", (roll_bottom, roll_bottom_total)
      ; "(top)  (in) roll", (roll_in_top, roll_in_top_total)
      ; "(middle)  (in) roll", (roll_in_middle, roll_in_middle_total)
      ; "(bottom)  (in) roll", (roll_in_bottom, roll_in_bottom_total)
      ; "(top) (out) roll", (roll_out_top, roll_out_top_total)
      ; "(middle) (out) roll", (roll_out_middle, roll_out_middle_total)
      ; "(bottom) (out) roll", (roll_out_bottom, roll_out_bottom_total)
      ]
  in
  let sections = [ "hand-finger", hf_table; "hand", hand_table ] in
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
  and roll = roll
  and roll_total = roll_total
  and roll_top = roll_top
  and roll_top_total = roll_top_total
  and roll_middle = roll_middle
  and roll_middle_total = roll_middle_total
  and roll_bottom = roll_bottom
  and roll_bottom_total = roll_bottom_total
  and roll_in = roll_in
  and roll_in_total = roll_in_total
  and roll_in_top = roll_in_top
  and roll_in_top_total = roll_in_top_total
  and roll_in_middle = roll_in_middle
  and roll_in_middle_total = roll_in_middle_total
  and roll_in_bottom = roll_in_bottom
  and roll_in_bottom_total = roll_in_bottom_total
  and roll_out = roll_out
  and roll_out_total = roll_out_total
  and roll_out_top = roll_out_top
  and roll_out_top_total = roll_out_top_total
  and roll_out_middle = roll_out_middle
  and roll_out_middle_total = roll_out_middle_total
  and roll_out_bottom = roll_out_bottom
  and roll_out_bottom_total = roll_out_bottom_total
  and dshrc = dshrc
  and dshrc_total = dshrc_total
  and dshrc_good = dshrc_good
  and dshrc_good_total = dshrc_good_total
  and dshrc_bad = dshrc_bad
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
  ; roll
  ; roll_total
  ; roll_top
  ; roll_top_total
  ; roll_middle
  ; roll_middle_total
  ; roll_bottom
  ; roll_bottom_total
  ; roll_in
  ; roll_in_total
  ; roll_in_top
  ; roll_in_top_total
  ; roll_in_middle
  ; roll_in_middle_total
  ; roll_in_bottom
  ; roll_in_bottom_total
  ; roll_out
  ; roll_out_total
  ; roll_out_top
  ; roll_out_top_total
  ; roll_out_middle
  ; roll_out_middle_total
  ; roll_out_bottom
  ; roll_out_bottom_total
  ; dshrc
  ; dshrc_good
  ; dshrc_bad
  ; dshrc_total
  ; dshrc_good_total
  ; dshrc_bad_total
  }
;;
