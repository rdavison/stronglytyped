open! Import

type t = float Incr.t

type weights =
  { sfb : Hand_finger.t -> float
  ; sfbs : float
  ; sfs : Hand_finger.t -> float
  ; sfss : float
  }

let default_weights : weights =
  { sfb = Fn.const 1.; sfbs = 1.; sfs = Fn.const 1.; sfss = 1. }
;;

let make (stats : Stats.t) ~(weights : weights) =
  let sfbs =
    stats.sfbs
    |> Map.to_alist
    |> List.to_array
    |> Array.map ~f:(fun (hf, v) ->
      let w = weights.sfb hf in
      Incr.map v ~f:(fun v -> v *. w))
    |> Incr.sum_float
    |> Incr.map ~f:(fun v -> v *. weights.sfbs)
  in
  let sfss =
    stats.sfss
    |> Map.to_alist
    |> List.to_array
    |> Array.map ~f:(fun (hf, v) ->
      let w = weights.sfb hf in
      Incr.map v ~f:(fun v -> v *. w))
    |> Incr.sum_float
    |> Incr.map ~f:(fun v -> v *. weights.sfss)
  in
  Incr.sum_float [| sfbs; sfss |]
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
  let weights = { default_weights with sfss = 0. } in
  let score = make stats ~weights in
  let observer = Incr.observe score in
  Incr.stabilize ();
  let score = Incr.Observer.value_exn observer in
  printf "%.4f" (score *. 100.);
  [%expect {| 2.3291 |}]
;;
