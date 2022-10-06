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
  end

  (* Unique finger *)
  module Uf = struct
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

    let good_or_bad =
      let good = T.good in
      let bad = T.bad in
      fun k1 k2 ~stagger ->
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
let roll = Roll.incr
let roll_total = Roll.total
let uf = Uf.incr
let uf_total = Uf.total
