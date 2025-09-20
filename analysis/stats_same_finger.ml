open! Core
open! Bonsai
open! Bonsai.Let_syntax

type ('breakdown, 'total) metric =
  { breakdown : 'breakdown Hand_finger.Map.t
  ; total : 'total
  }
[@@deriving sexp, compare, equal]

type t =
  | Sfb of (float, float) metric
  | Sfs of (float, float) metric
  | Speed of (float, float) metric
  | Sfb_worst of ((string * float) list * float, float) metric
  | Sfs_worst of ((string * float) list * float, float) metric
  | Speed_worst of ((string * float) list * float, float) metric
[@@deriving sexp, compare, equal, typed_variants]

let bigram (t : Typed_variant.Packed.t) (bigram_info : Bigram_data.info) =
  match t.f with
  | T Sfb -> bigram_info.freqs.ab
  | T Sfs -> bigram_info.freqs.axc
  | T Speed -> bigram_info.dist *. (bigram_info.freqs.ab +. (0.5 *. bigram_info.freqs.axc))
  | T Sfb_worst -> bigram_info.freqs.ab
  | T Sfs_worst -> bigram_info.freqs.axc
  | T Speed_worst ->
    bigram_info.dist *. (bigram_info.freqs.ab +. (0.5 *. bigram_info.freqs.axc))
;;

let component
      ~(metrics :
         (Typed_variant.Packed.t, Typed_variant.Packed.comparator_witness) Base.Set.t
           Bonsai.t)
      ~worst_counter
      ~diff_row_bigram_data
      graph
  =
  let simple (metric : (float, float) metric Typed_variant.t) graph =
    let freqs =
      Bonsai.assoc
        (module Hand_finger)
        diff_row_bigram_data
        ~f:(fun _hand_finger bigram_data graph ->
          Bonsai.Map.sum
            bigram_data
            (module Float)
            ~f:(bigram (Typed_variant.Packed.pack metric))
            graph)
        graph
    in
    let total = Bonsai.Map.sum freqs (module Float) ~f:Fn.id graph in
    let%arr breakdown = freqs
    and total = total in
    Typed_variant.create metric { breakdown; total }
  in
  let _prev_curr (metric : (float option * float, float) metric Typed_variant.t) graph =
    let freqs =
      Bonsai.assoc
        (module Hand_finger)
        diff_row_bigram_data
        ~f:(fun _hand_finger bigram_data graph ->
          let curr =
            Bonsai.Map.sum
              bigram_data
              (module Float)
              ~f:(bigram (Typed_variant.Packed.pack metric))
              graph
          in
          let prev = Bonsai.previous_value ~equal:Float.equal curr graph in
          Bonsai.both prev curr
          |> Bonsai.cutoff ~equal:(fun (_, a) (_, b) -> Float.equal a b))
        graph
    in
    let total =
      let curr =
        Bonsai.Map.sum freqs (module Float) ~f:(fun (_prev, curr) -> curr) graph
      in
      let _prev = () in
      curr
    in
    let%arr breakdown = freqs
    and total = total in
    Typed_variant.create metric { breakdown; total }
  in
  let detailed
        (metric : ((string * float) list * float, float) metric Typed_variant.t)
        graph
    =
    let worst_n =
      Bonsai.assoc
        (module Hand_finger)
        diff_row_bigram_data
        ~f:(fun _hand_finger bigram_data _graph ->
          let%arr worst_counter = worst_counter
          and bigram_data = bigram_data in
          let breakdown =
            bigram_data
            |> Map.to_alist
            |> List.map ~f:(fun (_, bigram_info) ->
              bigram_info.bigram, bigram (Typed_variant.Packed.pack metric) bigram_info)
            |> List.sort ~compare:(fun (_, a) (_, b) -> Float.compare b a)
            |> Fn.flip List.take worst_counter
          in
          let per_finger_total = List.sum (module Float) breakdown ~f:(fun (_, x) -> x) in
          breakdown, per_finger_total)
        graph
    in
    let worst_n_total =
      Bonsai.Map.sum
        worst_n
        (module Float)
        ~f:(fun (worst_bigrams, _per_finger_total) ->
          List.sum (module Float) worst_bigrams ~f:(fun (_bigram, freq) -> freq))
        graph
    in
    let%arr breakdown = worst_n
    and total = worst_n_total in
    Typed_variant.create metric { breakdown; total }
  in
  Bonsai.assoc_set
    (module Typed_variant.Packed)
    metrics
    ~f:(fun key graph ->
      match%sub key with
      | { f = T Sfb } -> simple Sfb graph
      | { f = T Sfs } -> simple Sfs graph
      | { f = T Speed } -> simple Speed graph
      | { f = T Sfb_worst } -> detailed Sfb_worst graph
      | { f = T Sfs_worst } -> detailed Sfs_worst graph
      | { f = T Speed_worst } -> detailed Speed_worst graph)
    graph
;;

let bigram_data bigram_data graph : Bigram_data.t Hand_finger.Map.t Bonsai.t =
  let same_hand_same_finger_bigram_data =
    Bonsai.Map.index_byi
      bigram_data
      ~comparator:(module Hand_finger)
      ~index:(fun ~key:(a, b) ~data:_ ->
        let x = Key.Id.hand a, Key.Id.finger a in
        if Hand_finger.equal x (Key.Id.hand b, Key.Id.finger b) then Some x else None)
      graph
  in
  Bonsai.assoc
    (module Hand_finger)
    same_hand_same_finger_bigram_data
    ~f:(fun _ (bigram_data : Bigram_data.t Bonsai.t) graph ->
      Bonsai.Map.filter_mapi
        bigram_data
        ~f:(fun ~key:(a, b) ~data ->
          if Int.equal (Key.Id.row a) (Key.Id.row b) then None else Some data)
        graph)
    graph
;;
