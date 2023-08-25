open! Import

type config =
  { usage : float Hand_finger.Map.t -> Hand_finger.t -> float -> float
  ; aggregate_usage :
      float Hand_finger.Map.t -> unweighted:float -> weighted:float -> float
  ; sfb : float Hand_finger.Map.t -> Hand_finger.t -> float -> float
  ; aggregate_sfb : float Hand_finger.Map.t -> unweighted:float -> weighted:float -> float
  ; sfs : float Hand_finger.Map.t -> Hand_finger.t -> float -> float
  ; aggregate_sfs : float Hand_finger.Map.t -> unweighted:float -> weighted:float -> float
  ; speed : float Hand_finger.Map.t -> Hand_finger.t -> float -> float
  ; aggregate_speed :
      float Hand_finger.Map.t -> unweighted:float -> weighted:float -> float
  ; inrowlls : float Hand.Map.t -> Hand.t -> float -> float
  ; aggregate_inrowlls : float Hand.Map.t -> unweighted:float -> weighted:float -> float
  ; outrowlls : float Hand.Map.t -> Hand.t -> float -> float
  ; aggregate_outrowlls : float Hand.Map.t -> unweighted:float -> weighted:float -> float
  ; scissors : float -> float
  ; lsb : float -> float
  ; slaps : float -> float
  }

type info =
  { unweighted : float
  ; weighted : float
  ; final : float
  }
[@@deriving sexp_of]

type t =
  { usage : info
  ; sfb : info
  ; sfs : info
  ; speed : info
  ; inrowlls : info
  ; outrowlls : info
  ; scissors : float
  ; lsb : float
  ; slaps : float
  }
[@@deriving sexp_of]

let default_config : config =
  { usage =
      (fun _map (hand, finger) v ->
        let w =
          match hand, finger with
          | `L, `P -> 1.1
          | `L, `R -> 1.
          | `L, `M -> 1.
          | `L, `I -> 1.
          | `L, `T -> 1.
          | `R, `T -> 1.
          | `R, `I -> 1.
          | `R, `M -> 1.
          | `R, `R -> 1.
          | `R, `P -> 1.1
        in
        v *. w)
  ; aggregate_usage =
      (fun map ->
        let get hf = Map.find map hf |> Option.value ~default:0. in
        let f p a b =
          let a = get a in
          let b = get b in
          if p a b then 0. else Float.abs (a -. b)
        in
        let lt a b = f Float.( < ) a b in
        let gt a b = f Float.( > ) a b in
        fun ~unweighted:_ ~weighted ->
          let res =
            [ lt (`L, `P) (`L, `R)
            ; lt (`L, `R) (`L, `M)
            ; gt (`L, `M) (`L, `I)
            ; lt (`R, `P) (`R, `R)
            ; lt (`R, `R) (`R, `M)
            ; gt (`R, `M) (`R, `I)
            ; lt (`L, `P) (`R, `R)
            ; lt (`L, `R) (`R, `M)
            ; gt (`L, `M) (`R, `I)
            ; lt (`R, `P) (`L, `R)
            ; lt (`R, `R) (`L, `M)
            ; gt (`R, `M) (`L, `I)
            ]
          in
          let mu = 1. +. List.sum (module Float) res ~f:Fn.id in
          Float.exp (mu *. weighted))
  ; sfb = (fun _map (_hand, _finger) v -> (if Float.( > ) v 0.235 then 10. else 3.) *. v)
  ; aggregate_sfb =
      (fun _map ~unweighted ~weighted ->
        if Float.( > ) unweighted 0.013 then Float.exp (10. *. weighted) else unweighted)
  ; sfs =
      (fun _map (hand, finger) v ->
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
  ; aggregate_sfs = (fun _map ~unweighted:_ ~weighted -> weighted)
  ; speed =
      (fun _map (hand, finger) v ->
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
  ; aggregate_speed =
      (fun map ->
        let get hf = Map.find map hf |> Option.value ~default:0. in
        let f p a b =
          let a = get a in
          let b = get b in
          if p a b then 0. else Float.abs (a -. b)
        in
        let lt a b = f Float.( < ) a b in
        let gt a b = f Float.( > ) a b in
        fun ~unweighted:_ ~weighted ->
          let res =
            [ lt (`L, `P) (`L, `R)
            ; lt (`L, `R) (`L, `M)
            ; gt (`L, `M) (`L, `I)
            ; lt (`R, `P) (`R, `R)
            ; lt (`R, `R) (`R, `M)
            ; gt (`R, `M) (`R, `I)
            ; lt (`L, `P) (`R, `R)
            ; lt (`L, `R) (`R, `M)
            ; gt (`L, `M) (`R, `I)
            ; lt (`R, `P) (`L, `R)
            ; lt (`R, `R) (`L, `M)
            ; gt (`R, `M) (`L, `I)
            ]
          in
          let mu = 1. +. List.sum (module Float) res ~f:Fn.id in
          Float.exp (mu *. weighted))
  ; inrowlls =
      (fun _map hand v ->
        let w =
          match hand with
          | `L -> 1.
          | `R -> 1.
        in
        v *. w)
  ; aggregate_inrowlls = (fun _map ~unweighted ~weighted:_ -> 1. -. unweighted)
  ; outrowlls =
      (fun _map hand v ->
        let w =
          match hand with
          | `L -> 1.
          | `R -> 1.
        in
        v *. w)
  ; aggregate_outrowlls = (fun _map ~unweighted ~weighted:_ -> 1. -. unweighted)
  ; scissors = (fun x -> 3. *. x)
  ; lsb = (fun x -> 3. *. x)
  ; slaps = (fun x -> 3. *. (1. -. x))
  }
;;

let make (stats : Stats.t) ~(config : config) =
  let map_stat of_alist_exn stat score_component score_aggregate =
    let components =
      let ks, vs = stat |> Map.to_alist |> List.unzip in
      Incr.all vs |> Incr.map ~f:(fun vs -> List.zip_exn ks vs |> of_alist_exn)
    in
    let unweighted =
      components
      |> Incr.map ~f:(fun map ->
        Map.to_alist map |> List.map ~f:snd |> List.sum (module Float) ~f:Fn.id)
    in
    let weighted =
      components
      |> Incr.map ~f:(fun map ->
        Map.to_alist map
        |> List.map ~f:(fun (k, v) -> score_component map k v)
        |> List.sum (module Float) ~f:Fn.id)
    in
    Incr.map3 components unweighted weighted ~f:(fun map unweighted weighted ->
      let final = score_aggregate map ~unweighted ~weighted in
      { unweighted; weighted; final })
  in
  let usage =
    map_stat Hand_finger.Map.of_alist_exn stats.usage config.usage config.aggregate_usage
  in
  let sfbs =
    map_stat Hand_finger.Map.of_alist_exn stats.sfbs config.sfb config.aggregate_sfb
  in
  let sfss =
    map_stat Hand_finger.Map.of_alist_exn stats.sfss config.sfs config.aggregate_sfs
  in
  let speeds =
    map_stat Hand_finger.Map.of_alist_exn stats.speed config.speed config.aggregate_speed
  in
  let inrowlls =
    map_stat
      Hand.Map.of_alist_exn
      stats.inrowlls
      config.inrowlls
      config.aggregate_inrowlls
  in
  let outrowlls =
    map_stat
      Hand.Map.of_alist_exn
      stats.outrowlls
      config.outrowlls
      config.aggregate_outrowlls
  in
  let scissors = Incr.map stats.scissors ~f:config.scissors in
  let lsb = Incr.map stats.lsb ~f:config.lsb in
  let slaps = Incr.map stats.slaps ~f:config.slaps in
  let%map_open.Incr usage = usage
  and sfb = sfbs
  and sfs = sfss
  and speed = speeds
  and inrowlls = inrowlls
  and outrowlls = outrowlls
  and scissors = scissors
  and lsb = lsb
  and slaps = slaps in
  { usage; sfb; sfs; speed; inrowlls; outrowlls; scissors; lsb; slaps }
;;

let final_sum (t : t Incr.t) =
  Incr.map t ~f:(fun score ->
    score.inrowlls.final
    +. score.usage.final
    +. score.speed.final
    +. score.sfb.final
    +. score.scissors
    +. score.lsb
    +. score.slaps)
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
  let score = final_sum (make stats ~config) in
  let observer = Incr.observe score in
  Incr.stabilize ();
  let score = Incr.Observer.value_exn observer in
  printf "%.4f" (score *. 100.);
  [%expect {| 2.3581 |}]
;;
