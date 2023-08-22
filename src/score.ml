open! Import

type t = float Incr.t

type config =
  { usage : Hand_finger.t -> float -> float
  ; aggregate_usage : unweighted:float -> weighted:float -> float
  ; sfb : Hand_finger.t -> float -> float
  ; aggregate_sfb : unweighted:float -> weighted:float -> float
  ; sfs : Hand_finger.t -> float -> float
  ; aggregate_sfs : unweighted:float -> weighted:float -> float
  ; speed : Hand_finger.t -> float -> float
  ; aggregate_speed : unweighted:float -> weighted:float -> float
  ; inrowlls : Hand.t -> float -> float
  ; aggregate_inrowlls : unweighted:float -> weighted:float -> float
  ; outrowlls : Hand.t -> float -> float
  ; aggregate_outrowlls : unweighted:float -> weighted:float -> float
  }

let default_config : config =
  let open! Float in
  { usage =
      (fun (hand, finger) v ->
        let w =
          match hand, finger with
          | `L, `P -> 1.
          | `L, `R -> 1.
          | `L, `M -> 1.
          | `L, `I -> 1.
          | `L, `T -> 1.
          | `R, `T -> 1.
          | `R, `I -> 1.
          | `R, `M -> 1.
          | `R, `R -> 1.
          | `R, `P -> 1.
        in
        v *. w)
  ; aggregate_usage = (fun ~unweighted:_ ~weighted -> weighted)
  ; sfb =
      (fun (hand, finger) v ->
        let w =
          match hand, finger with
          | `L, `P -> 1.
          | `L, `R -> 1.
          | `L, `M -> 1.
          | `L, `I -> 1.
          | `L, `T -> 1.
          | `R, `T -> 1.
          | `R, `I -> 1.
          | `R, `M -> 1.
          | `R, `R -> 1.
          | `R, `P -> 1.
        in
        v *. w)
  ; aggregate_sfb = (fun ~unweighted:_ ~weighted -> weighted)
  ; sfs =
      (fun (hand, finger) v ->
        let w =
          match hand, finger with
          | `L, `P -> 1.
          | `L, `R -> 1.
          | `L, `M -> 1.
          | `L, `I -> 1.
          | `L, `T -> 1.
          | `R, `T -> 1.
          | `R, `I -> 1.
          | `R, `M -> 1.
          | `R, `R -> 1.
          | `R, `P -> 1.
        in
        v *. w)
  ; aggregate_sfs = (fun ~unweighted:_ ~weighted -> weighted)
  ; speed =
      (fun (hand, finger) v ->
        let w =
          match hand, finger with
          | `L, `P -> 1.
          | `L, `R -> 1.
          | `L, `M -> 1.
          | `L, `I -> 1.
          | `L, `T -> 1.
          | `R, `T -> 1.
          | `R, `I -> 1.
          | `R, `M -> 1.
          | `R, `R -> 1.
          | `R, `P -> 1.
        in
        v *. w)
  ; aggregate_speed = (fun ~unweighted:_ ~weighted -> weighted)
  ; inrowlls =
      (fun hand v ->
        let w =
          match hand with
          | `L -> 1.
          | `R -> 1.
        in
        v *. w)
  ; aggregate_inrowlls = (fun ~unweighted:_ ~weighted -> weighted)
  ; outrowlls =
      (fun hand v ->
        let w =
          match hand with
          | `L -> 1.
          | `R -> 1.
        in
        v *. w)
  ; aggregate_outrowlls = (fun ~unweighted:_ ~weighted -> weighted)
  }
;;

let make (stats : Stats.t) ~(config : config) =
  let map_stat stat score_component score_aggregate =
    let components = stat |> Map.to_alist |> List.to_array in
    let unweighted = components |> Array.map ~f:(fun (_, v) -> v) |> Incr.sum_float in
    let weighted =
      components
      |> Array.map ~f:(fun (k, v) -> Incr.map v ~f:(fun v -> score_component k v))
      |> Incr.sum_float
    in
    Incr.map2 unweighted weighted ~f:(fun unweighted weighted ->
      score_aggregate ~unweighted ~weighted)
  in
  let usage = map_stat stats.usage config.usage config.aggregate_usage in
  let sfbs = map_stat stats.sfbs config.sfb config.aggregate_sfb in
  let sfss = map_stat stats.sfss config.sfs config.aggregate_sfs in
  let speeds = map_stat stats.speed config.speed config.aggregate_speed in
  let inrowlls = map_stat stats.inrowlls config.inrowlls config.aggregate_inrowlls in
  let outrowlls = map_stat stats.outrowlls config.outrowlls config.aggregate_outrowlls in
  Incr.sum_float [| usage; sfbs; sfss; speeds; inrowlls; outrowlls |]
;;

let%expect_test "graphite" =
  let corpus =
    let data =
      In_channel.read_all
        (match Sites.Sites.corpus with
         | [ path ] -> path ^/ "typeracer"
         | _ -> failwith "No path to corpus")
    in
    let sexp = Sexp.of_string data in
    Corpus.t_of_sexp sexp
  in
  let layout = Layout.ortho42 () in
  let stats = Stats.make layout corpus in
  let config = default_config in
  let score = make stats ~config in
  let observer = Incr.observe score in
  Incr.stabilize ();
  let score = Incr.Observer.value_exn observer in
  printf "%.4f" (score *. 100.);
  [%expect {| 2.3581 |}]
;;
