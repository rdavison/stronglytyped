open! Core
open! Bonsai
open! Bonsai.Let_syntax

module Hand_finger = struct
  type ('breakdown, 'total) t =
    { breakdown : 'breakdown Hand_finger.Map.t
    ; total : 'total
    }

  let diff_row_bigram_data ~bigram_data graph : Bigram_data.t Hand_finger.Map.t Bonsai.t =
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
      graph
      ~f:(fun _ (bigram_data : Bigram_data.t Bonsai.t) graph ->
        Bonsai.Map.filter_mapi
          bigram_data
          ~f:(fun ~key:(a, b) ~data ->
            if Int.equal (Key.Id.row a) (Key.Id.row b) then None else Some data)
          graph)
  ;;

  let make
        ~(diff_row_bigram_data : Bigram_data.t Hand_finger.Map.t Bonsai.t)
        ~(compute_metric : Bigram_data.info -> float)
        graph
    =
    let freqs =
      Bonsai.assoc
        (module Hand_finger)
        diff_row_bigram_data
        ~f:(fun _hand_finger bigram_data graph ->
          Bonsai.Map.sum bigram_data (module Float) ~f:compute_metric graph)
        graph
    in
    let total = Bonsai.Map.sum freqs (module Float) ~f:Fn.id graph in
    let%arr breakdown = freqs
    and total = total in
    { breakdown; total }
  ;;

  let worst
        ~(diff_row_bigram_data : Bigram_data.t Hand_finger.Map.t Bonsai.t)
        ~(worst_counter : int Bonsai.t)
        ~(compute_metric : Bigram_data.info -> float)
        graph
    =
    let worst_n =
      Bonsai.assoc
        (module Hand_finger)
        diff_row_bigram_data
        ~f:(fun _hand_finger bigram_data _graph ->
          let%arr worst_counter = worst_counter
          and bigram_data = bigram_data in
          bigram_data
          |> Map.to_alist
          |> List.map ~f:(fun (_, bigram_info) ->
            bigram_info.bigram, compute_metric bigram_info)
          |> List.sort ~compare:(fun (_, a) (_, b) -> Float.compare b a)
          |> Fn.flip List.take worst_counter)
        graph
    in
    let worst_n_total =
      Bonsai.Map.sum
        worst_n
        (module Float)
        ~f:(fun worst_bigrams ->
          List.sum (module Float) worst_bigrams ~f:(fun (_bigram, freq) -> freq))
        graph
    in
    let%arr breakdown = worst_n
    and total = worst_n_total in
    { breakdown; total }
  ;;

  let sfb ~diff_row_bigram_data graph =
    make
      ~diff_row_bigram_data
      ~compute_metric:(fun (bigram_info : Bigram_data.info) -> bigram_info.freqs.ab)
      graph
  ;;

  let sfs ~diff_row_bigram_data graph =
    make
      ~diff_row_bigram_data
      ~compute_metric:(fun (bigram_info : Bigram_data.info) -> bigram_info.freqs.axc)
      graph
  ;;

  let speed ~diff_row_bigram_data graph =
    make
      ~diff_row_bigram_data
      ~compute_metric:(fun (bigram_info : Bigram_data.info) ->
        bigram_info.dist *. (bigram_info.freqs.ab +. (0.5 *. bigram_info.freqs.axc)))
      graph
  ;;

  let sfb_worst ~diff_row_bigram_data ~worst_counter graph =
    worst
      ~diff_row_bigram_data
      ~worst_counter
      ~compute_metric:(fun (bigram_info : Bigram_data.info) -> bigram_info.freqs.ab)
      graph
  ;;

  let sfs_worst ~diff_row_bigram_data ~worst_counter graph =
    worst
      ~diff_row_bigram_data
      ~worst_counter
      ~compute_metric:(fun (bigram_info : Bigram_data.info) -> bigram_info.freqs.axc)
      graph
  ;;

  let speed_worst ~diff_row_bigram_data ~worst_counter graph =
    worst
      ~diff_row_bigram_data
      ~worst_counter
      ~compute_metric:(fun (bigram_info : Bigram_data.info) ->
        bigram_info.dist *. (bigram_info.freqs.ab +. (0.5 *. bigram_info.freqs.axc)))
      graph
  ;;
end
