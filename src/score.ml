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
    ; shb : info option
    ; sfs : info option
    ; shs : info option
    ; speed : info option
    ; fsb : info option
    ; hsb : info option
    ; fss : info option
    ; hss : info option
    ; lsb : info option
    ; lss : info option
    ; srb : info option
    }
  [@@deriving sexp_of]

  let make
    ?usage
    ?sfb
    ?shb
    ?sfs
    ?shs
    ?speed
    ?fsb
    ?hsb
    ?fss
    ?hss
    ?lsb
    ?lss
    ?srb
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
    let _simple param stat =
      match param with
      | None -> Incr.return None
      | Some f -> Incr.map stat ~f:(Fn.compose Option.some f)
    in
    let%map_open.Incr usage = map usage stats.usage Hand_finger.Map.of_alist_exn
    and sfb = map sfb stats.sfb Hand_finger.Map.of_alist_exn
    and shb = map shb stats.shb Hand.Map.of_alist_exn
    and sfs = map sfs stats.sfs Hand_finger.Map.of_alist_exn
    and shs = map shs stats.shs Hand.Map.of_alist_exn
    and speed = map speed stats.speed Hand_finger.Map.of_alist_exn
    and fsb = map fsb stats.fsb Hand_finger2.Map.of_alist_exn
    and hsb = map hsb stats.hsb Hand_finger2.Map.of_alist_exn
    and fss = map fss stats.fss Hand_finger2.Map.of_alist_exn
    and hss = map hss stats.hss Hand_finger2.Map.of_alist_exn
    and lsb = map lsb stats.lsb Hand_finger2.Map.of_alist_exn
    and lss = map lss stats.lss Hand_finger2.Map.of_alist_exn
    and srb = map srb stats.srb Hand_finger2.Map.of_alist_exn in
    { usage; sfb; shb; sfs; shs; speed; fsb; hsb; fss; hss; lsb; lss; srb }
  ;;

  let default_config =
    make
      ~usage:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let final =
          let get hf = Map.find map hf |> Option.value ~default:0. in
          let f p a b =
            let a = get a in
            let b = get b in
            if p a b then 0. else Float.abs (a -. b)
          in
          let lt_f a b = f Float.( < ) a b in
          let lt_q a b =
            let a = get a in
            if Float.( < ) a b then 0. else 1. +. Float.abs (a -. b)
          in
          (* let gt a b = f Float.( > ) a b in *)
          let res =
            [ lt_f (`L, `P) (`L, `R)
            ; lt_f (`L, `P) (`L, `M)
            ; lt_f (`L, `P) (`L, `I)
            ; lt_f (`L, `R) (`L, `M)
            ; lt_f (`L, `R) (`L, `I)
            ; lt_f (`R, `P) (`R, `R)
            ; lt_f (`R, `P) (`R, `M)
            ; lt_f (`R, `P) (`R, `I)
            ; lt_f (`R, `R) (`R, `M)
            ; lt_f (`R, `R) (`R, `I)
            ; lt_q (`L, `P) 0.08
            ; lt_q (`R, `P) 0.08
            ; lt_q (`L, `R) 0.1
            ; lt_q (`R, `R) 0.1
            ; lt_q (`L, `M) 0.11
            ]
          in
          Float.exp (1. +. List.sum (module Float) res ~f:Fn.id)
        in
        { unweighted; weighted = final })
      ~sfb:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let final =
          let get hf = Map.find map hf |> Option.value ~default:0. in
          let f p a b =
            let a = get a in
            let b = get b in
            if p a b then 0. else Float.abs (a -. b)
          in
          let lt_f a b = f Float.( < ) a b in
          let lt_q (h, f) b =
            let a = get (h, f) in
            if Float.( < ) a b then 0. else 1. +. Float.abs (a -. b)
          in
          (* let gt a b = f Float.( > ) a b in *)
          let res =
            [ lt_f (`L, `P) (`L, `R)
            ; lt_f (`L, `P) (`L, `M)
            ; lt_f (`L, `P) (`L, `I)
            ; lt_q (`L, `M) 0.0022
            ; lt_q (`L, `I) 0.0022
            ; lt_q (`R, `I) 0.0022
            ; lt_q (`R, `P) 0.0022
            ]
          in
          Float.exp ((1. +. List.sum (module Float) res ~f:Fn.id) *. 2.)
        in
        { unweighted; weighted = final })
      ~shb:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let weighted = Float.exp (1. +. unweighted) in
        { unweighted; weighted })
      ~sfs:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let final =
          let get hf = Map.find map hf |> Option.value ~default:0. in
          let f p a b =
            let a = get a in
            let b = get b in
            if p a b then 0. else Float.abs (a -. b)
          in
          let lt_f a b = f Float.( < ) a b in
          let lt_q (h, f) b =
            let a = get (h, f) in
            if Float.( < ) a b then 0. else 1. +. Float.abs (a -. b)
          in
          (* let gt a b = f Float.( > ) a b in *)
          let res =
            [ lt_f (`L, `P) (`L, `R)
            ; lt_f (`L, `P) (`L, `M)
            ; lt_f (`L, `P) (`L, `I)
            ; lt_q (`L, `P) 0.001
            ; lt_q (`L, `R) 0.0021
            ; lt_q (`L, `M) 0.0033
            ; lt_q (`L, `I) 0.0093
            ]
          in
          Float.exp (1. +. List.sum (module Float) res ~f:Fn.id)
        in
        { unweighted; weighted = final })
      ~shs:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let weighted = Float.exp (1. +. (1. -. unweighted)) in
        { unweighted; weighted })
      ~speed:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let final =
          let get hf = Map.find map hf |> Option.value_exn in
          let f p a b =
            let a = get a in
            let b = get b in
            if p a b then 0. else Float.abs (a -. b)
          in
          let lt_f a b = f Float.( < ) a b in
          let lt_q a b =
            let a = get a in
            if Float.( < ) a b then 0. else 1. +. Float.abs (a -. b)
          in
          (* let gt a b = f Float.( > ) a b in *)
          let res =
            [ lt_f (`L, `P) (`L, `R)
            ; lt_f (`L, `R) (`L, `M)
            ; lt_f (`L, `M) (`L, `I)
            ; lt_f (`L, `P) (`R, `R)
            ; lt_f (`L, `R) (`R, `M)
            ; lt_f (`L, `M) (`R, `I)
            ; lt_f (`R, `P) (`L, `R)
            ; lt_f (`R, `R) (`L, `M)
            ; lt_f (`R, `M) (`L, `I)
            ; lt_f (`R, `P) (`R, `R)
            ; lt_f (`R, `R) (`R, `M)
            ; lt_f (`R, `M) (`R, `I)
            ; lt_q (`L, `I) 0.50
            ; lt_q (`R, `I) 0.50
              (* ; lt_q (`L, `P) 0.0004
                 ; lt_q (`L, `R) 0.0012
                 ; lt_q (`L, `M) 0.0024
                 ; lt_q (`L, `I) 0.0077 *)
            ]
          in
          Float.exp (1. +. List.sum (module Float) res ~f:Fn.id)
        in
        { unweighted; weighted = final })
      ~hsb:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let weighted = 2. *. Float.exp (1. +. unweighted) in
        { unweighted; weighted })
      ~hss:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let weighted = 1. *. Float.exp (1. +. unweighted) in
        { unweighted; weighted })
      ~fsb:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let weighted = 4. *. Float.exp (1. +. unweighted) in
        { unweighted; weighted })
      ~fss:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let weighted = 3. *. Float.exp (1. +. unweighted) in
        { unweighted; weighted })
      ~lsb:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let weighted = Float.exp (1. +. unweighted) in
        { unweighted; weighted })
      ~lss:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let weighted = 0.5 *. Float.exp (1. +. unweighted) in
        { unweighted; weighted })
      ~srb:(fun map ->
        let unweighted = Map.fold map ~init:0. ~f:(fun ~key:_ ~data:v acc -> acc +. v) in
        let weighted = Float.exp (1. +. (1. -. unweighted)) in
        { unweighted; weighted })
  ;;

  let final_sum (t : t Incr.t) =
    Incr.map
      t
      ~f:(fun { usage; sfb; shb; sfs; shs; speed; fsb; hsb; fss; hss; lsb; lss; srb } ->
        let sum =
          [ usage; sfb; shb; sfs; shs; speed; fsb; hsb; fss; hss; lsb; lss; srb ]
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
