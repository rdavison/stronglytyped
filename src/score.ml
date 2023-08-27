open! Import
include Score_intf

module Make
    (Incr : Incremental.S)
    (Layout : Layout.S with module Incr = Incr)
    (Stats : Stats.S with module Incr = Incr and module Layout = Layout) :
  S with module Incr = Incr and module Layout = Layout and module Stats = Stats = struct
  module Incr = Incr
  module Layout = Layout
  module Stats = Stats

  type info =
    { unweighted : float
    ; weighted : float
    }
  [@@deriving sexp_of]

  type t =
    { usage : info option
    ; sfb : info option
    ; sfs : info option
    ; speed : info option
    ; inrowlls : info option
    ; outrowlls : info option
    ; scissors : info option
    ; lsb : info option
    ; slaps : info option
    ; badredirs : info option
    ; badtrills : info option
    ; layer_transitions : info option
    ; layer_trigger_s129 : info option
    }
  [@@deriving sexp_of]

  let make
    ?usage
    ?sfb
    ?sfs
    ?speed
    ?inrowlls
    ?outrowlls
    ?scissors
    ?lsb
    ?slaps
    ?badredirs
    ?badtrills
    ?layer_transitions
    ?layer_trigger_s129
    (stats : Stats.t)
    =
    let map param stat of_alist_exn =
      match param with
      | None -> Incr.return None
      | Some f ->
        stat
        |> Map.to_alist
        |> List.map ~f:(fun (k, incr) -> Incr.map incr ~f:(fun v -> k, v))
        |> Incr.all
        |> Incr.map ~f:of_alist_exn
        |> Incr.map ~f:(Fn.compose Option.some f)
    in
    let simple param stat =
      match param with
      | None -> Incr.return None
      | Some f -> Incr.map stat ~f:(Fn.compose Option.some f)
    in
    let%map_open.Incr usage = map usage stats.usage Hand_finger.Map.of_alist_exn
    and sfb = map sfb stats.sfbs Hand_finger.Map.of_alist_exn
    and sfs = map sfs stats.sfss Hand_finger.Map.of_alist_exn
    and speed = map speed stats.speed Hand_finger.Map.of_alist_exn
    and inrowlls = map inrowlls stats.inrowlls Hand.Map.of_alist_exn
    and outrowlls = map outrowlls stats.outrowlls Hand.Map.of_alist_exn
    and scissors = simple scissors stats.scissors
    and lsb = simple lsb stats.lsb
    and slaps = simple slaps stats.slaps
    and badredirs = simple badredirs stats.badredirs
    and badtrills = simple badtrills stats.badtrills
    and layer_transitions = simple layer_transitions stats.layer_transitions
    and layer_trigger_s129 = simple layer_trigger_s129 stats.layer_trigger_s129 in
    { usage
    ; sfb
    ; sfs
    ; speed
    ; inrowlls
    ; outrowlls
    ; scissors
    ; lsb
    ; slaps
    ; badredirs
    ; badtrills
    ; layer_transitions
    ; layer_trigger_s129
    }
  ;;

  let default_config =
    make
      ~usage:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let weighted =
          Map.fold map ~init:0. ~f:(fun ~key:(hand, finger) ~data:v acc ->
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
            acc +. (v *. w))
        in
        let final =
          let get hf = Map.find map hf |> Option.value ~default:0. in
          let f p a b =
            let a = get a in
            let b = get b in
            if p a b then 0. else Float.abs (a -. b)
          in
          let lt a b = f Float.( < ) a b in
          let gt a b = f Float.( > ) a b in
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
          Float.exp (mu *. weighted)
        in
        { unweighted; weighted = final })
      ~sfb:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let weighted =
          Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc ->
            acc +. (v *. if Float.( > ) v 0.00229 then 10. else 3.))
        in
        let final =
          if Float.( > ) unweighted 0.013 then Float.exp (10. *. weighted) else weighted
        in
        { unweighted; weighted = final })
      ?sfs:
        (let _ =
           fun map ->
           let unweighted =
             Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v)
           in
           let weighted = unweighted in
           { unweighted; weighted }
         in
         None)
      ~speed:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let weighted =
          Map.fold map ~init:0. ~f:(fun ~key:(_hand, finger) ~data:v acc ->
            let w =
              match finger with
              | `P -> if Float.( > ) v 0.0023 then 10. else 3.
              | _ -> if Float.( > ) v 0.00872 then 10. else 3.
            in
            acc +. (v *. w))
        in
        let final =
          let get hf = Map.find map hf |> Option.value ~default:0. in
          let f p a b =
            let a = get a in
            let b = get b in
            if p a b then 0. else Float.abs (a -. b)
          in
          let lt a b = f Float.( < ) a b in
          let gt a b = f Float.( > ) a b in
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
          Float.exp (2. *. mu *. weighted)
        in
        { unweighted; weighted = final })
      ~inrowlls:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let final = 1. -. unweighted in
        { unweighted; weighted = final })
      ?outrowlls:
        (let _ =
           fun map ->
           let unweighted =
             Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v)
           in
           let final = 1. -. unweighted in
           { unweighted; weighted = final }
         in
         None)
      ~scissors:(fun unweighted ->
        let weighted = 3. *. unweighted in
        { unweighted; weighted })
      ~lsb:(fun unweighted ->
        let weighted = 3. *. unweighted in
        { unweighted; weighted })
      ~slaps:(fun unweighted ->
        let weighted = 3. *. (1. -. unweighted) in
        { unweighted; weighted })
      ~layer_transitions:(fun unweighted ->
        let weighted = Float.exp (3. *. unweighted) in
        { unweighted; weighted })
      ~layer_trigger_s129:(fun unweighted ->
        let weighted = Float.exp (3. *. unweighted) in
        { unweighted; weighted })
      ?badredirs:
        (let _ =
           fun unweighted ->
           let weighted = 3000. *. unweighted in
           { unweighted; weighted }
         in
         None)
      ?badtrills:
        (let _ =
           fun unweighted ->
           let weighted = 3000. *. unweighted in
           { unweighted; weighted }
         in
         None)
  ;;

  let final_sum (t : t Incr.t) =
    Incr.map
      t
      ~f:
        (fun
          { usage
          ; sfb
          ; sfs
          ; speed
          ; inrowlls
          ; outrowlls
          ; scissors
          ; lsb
          ; slaps
          ; badredirs
          ; badtrills
          ; layer_transitions
          ; layer_trigger_s129
          }
        ->
        ignore sfs;
        ignore outrowlls;
        let sum =
          [ usage
          ; speed
          ; sfb
          ; scissors
          ; inrowlls
          ; lsb
          ; slaps
          ; badredirs
          ; badtrills
          ; layer_transitions
          ; layer_trigger_s129
          ]
          |> List.fold ~init:0. ~f:(fun acc info ->
            acc +. Option.value_map info ~f:(fun info -> info.weighted) ~default:0.)
        in
        sum)
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
    let score = final_sum (default_config stats) in
    let observer = Incr.observe score in
    Incr.stabilize ();
    let score = Incr.Observer.value_exn observer in
    printf "%.4f" (score *. 100.);
    [%expect {| 2.3581 |}]
  ;;
end
