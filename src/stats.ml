open! Import
include Stats_intf

module Make (Incr : Incremental.S) (Layout : Layout.S with module Incr = Incr) :
  S with module Incr = Incr and module Layout = Layout = struct
  module Incr = Incr
  module Layout = Layout

  type t =
    { usage : float Incr.t Hand_finger.Map.t
    ; sfbs : float Incr.t Hand_finger.Map.t
    ; sfss : float Incr.t Hand_finger.Map.t
    ; speed : float Incr.t Hand_finger.Map.t
    ; inrowlls : float Incr.t Hand.Map.t
    ; outrowlls : float Incr.t Hand.Map.t
    ; scissors : float Incr.t
    ; lsb : float Incr.t
    ; slaps : float Incr.t
    ; badredirs : float Incr.t
    ; badtrills : float Incr.t
    ; layer_transitions : float Incr.t
    ; layer_trigger_s129 : float Incr.t
    }
  [@@deriving sexp_of]

  let s29 = Hashtbl.create (module String)

  let preferred_rowchange_slope_dir f1 f2 =
    match f1, f2 with
    | `P, `R | `R, `P -> Some 1
    | `P, `M | `M, `P -> Some 1
    | `P, `I | `I, `P -> Some (-1)
    | `R, `M | `M, `R -> Some 1
    | `R, `I | `I, `R -> Some (-1)
    | `M, `I | `I, `M -> Some (-1)
    | (_ : Finger.t), (_ : Finger.t) -> None
  ;;

  let finger_dist f1 f2 =
    match f1, f2 with
    | `P, `R | `R, `P -> Some 1.
    | `P, `M | `M, `P -> Some 2.
    | `P, `I | `I, `P -> Some 3.
    | `R, `M | `M, `R -> Some 1.
    | `R, `I | `I, `R -> Some 2.
    | `M, `I | `I, `M -> Some 1.
    | (_ : Finger.t), (_ : Finger.t) -> None
  ;;

  let make (layout_ : Layout.t) (corpus : Corpus.t) =
    let layout = Array.map layout_.keys ~f:(fun (key, var) -> key, Incr.Var.watch var) in
    let ( usage
        , ( sfbs
          , sfss
          , speed
          , inrowlls
          , outrowlls
          , scissors
          , lsb
          , slaps
          , layer_transitions
          , layer_trigger_s129
          , (badredirs, badtrills) ) )
      =
      let init =
        let usage = Hand_finger.Map.empty in
        let bigram_stats =
          let sfbs = Hand_finger.Map.empty in
          let sfss = Hand_finger.Map.empty in
          let speed = Hand_finger.Map.empty in
          let inrowlls = Hand.Map.empty in
          let outrowlls = Hand.Map.empty in
          let scissors = [] in
          let lsb = [] in
          let slaps = [] in
          let layer_transitions = [] in
          let layer_trigger_s129 = [] in
          let trigram_stats =
            let badredirs = [] in
            let badtrills = [] in
            badredirs, badtrills
          in
          ( sfbs
          , sfss
          , speed
          , inrowlls
          , outrowlls
          , scissors
          , lsb
          , slaps
          , layer_transitions
          , layer_trigger_s129
          , trigram_stats )
        in
        usage, bigram_stats
      in
      Array.foldi layout ~init ~f:(fun i (usage, bigrams_stats) (k1, var1) ->
        let _l1, o1 = Layout.layer_offset layout_ i in
        let hf1 = Key.hand_finger k1 in
        let monogram = Incr.map var1 ~f:(fun v1 -> v1.code) in
        let freq_mono = Incr.map monogram ~f:(Corpus.Lookup.freq1 ~data:corpus.singles) in
        let usage =
          Map.update usage hf1 ~f:(function
            | None -> [ freq_mono ]
            | Some freq -> freq_mono :: freq)
        in
        let bigrams_stats =
          Array.foldi
            layout
            ~init:bigrams_stats
            ~f:
              (fun
                j
                ( sfbs
                , sfss
                , speed
                , inrowlls
                , outrowlls
                , scissors
                , lsb
                , slaps
                , layer_transitions
                , layer_trigger_s129
                , trigram_stats )
                (k2, var2)
              ->
              let _l2, o2 = Layout.layer_offset layout_ j in
              let hf2 = Key.hand_finger k2 in
              let slope = (k2.y -. k1.y) /. (k2.x -. k1.x) in
              let dist = Float.sqrt (((k1.y -. k2.y) ** 2.) +. ((k1.x -. k2.x) ** 2.)) in
              let dist' =
                Float.sqrt
                  (((k1.y -. k2.y) ** 2.)
                   +. ((k1.x
                        -. k2.x
                        -. (finger_dist k1.finger k2.finger |> Option.value ~default:0.))
                       ** 2.))
              in
              let bigram =
                Incr.both var1 var2 |> Incr.map ~f:(fun (v1, v2) -> v1.code, v2.code)
              in
              let freq_s1 = Incr.map bigram ~f:(Corpus.Lookup.freq2 ~data:corpus.s1) in
              let freq_s29 =
                Incr.map bigram ~f:(fun ((c1, c2) as bigram) ->
                  let bigram' = Code.to_string c1 ^ Code.to_string c2 in
                  Hashtbl.find_or_add s29 bigram' ~default:(fun () ->
                    [ Corpus.Lookup.freq2 bigram ~data:corpus.s2
                    ; Corpus.Lookup.freq2 bigram ~data:corpus.s3
                    ; Corpus.Lookup.freq2 bigram ~data:corpus.s4
                    ; Corpus.Lookup.freq2 bigram ~data:corpus.s5
                    ; Corpus.Lookup.freq2 bigram ~data:corpus.s6
                    ; Corpus.Lookup.freq2 bigram ~data:corpus.s7
                    ; Corpus.Lookup.freq2 bigram ~data:corpus.s8
                    ; Corpus.Lookup.freq2 bigram ~data:corpus.s9
                    ]
                    |> List.foldi ~init:0. ~f:(fun i acc sn ->
                      acc +. (sn /. Float.exp (Float.of_int (i + 1))))))
              in
              let sfbs =
                if Hand_finger.Infix.(hf1 = hf2) && o1 <> o2
                then
                  Map.update sfbs hf1 ~f:(function
                    | None -> [ freq_s1 ]
                    | Some freqs -> freq_s1 :: freqs)
                else sfbs
              in
              let sfss =
                if Hand_finger.Infix.(hf1 = hf2) && o1 <> o2
                then
                  Map.update sfss hf1 ~f:(function
                    | None -> [ freq_s29 ]
                    | Some freqs -> freq_s29 :: freqs)
                else sfss
              in
              let speed =
                if Hand_finger.Infix.(hf1 = hf2) && o1 <> o2
                then (
                  let speed' =
                    Incr.map2 freq_s1 freq_s29 ~f:(fun freq_s1 freq_s29 ->
                      dist *. (freq_s1 +. freq_s29))
                  in
                  Map.update speed hf1 ~f:(function
                    | None -> [ speed' ]
                    | Some speeds -> speed' :: speeds))
                else speed
              in
              let inrowlls =
                if Hand.equal k1.hand k2.hand
                   && k1.row = k2.row
                   && Finger.to_int k1.finger < Finger.to_int k2.finger
                then
                  Map.update inrowlls k1.hand ~f:(function
                    | None -> [ freq_s1 ]
                    | Some freqs -> freq_s1 :: freqs)
                else inrowlls
              in
              let outrowlls =
                if Hand.equal k1.hand k2.hand
                   && k1.row = k2.row
                   && Finger.to_int k1.finger > Finger.to_int k2.finger
                then
                  Map.update outrowlls k1.hand ~f:(function
                    | None -> [ freq_s1 ]
                    | Some freqs -> freq_s1 :: freqs)
                else outrowlls
              in
              let slaps =
                if Hand_finger.adjacent_no_thumb hf1 hf2
                   && Float.equal dist 1.
                   && k1.layer = k2.layer
                   && k1.row = k2.row
                then
                  Incr.map2 bigram freq_s1 ~f:(fun (c1, c2) v ->
                    match k1.finger, k2.finger with
                    | `P, `R | `R, `P | `R, `M | `M, `R ->
                      (match
                         Code.to_string c1 ^ Code.to_string c2 |> String.lowercase
                       with
                       | "mo"
                       | "om"
                       | "ar"
                       | "ra"
                       | "ec"
                       | "ce"
                       | "is"
                       | "si"
                       | "ey"
                       | "ye"
                       | "el"
                       | "le"
                       | "et"
                       | "te"
                       | "il"
                       | "li"
                       | "aw"
                       | "wa"
                       | "ta"
                       | "at"
                       | "de"
                       | "ed"
                       | "iv"
                       | "vi"
                       | "ag"
                       | "ga"
                       | "di"
                       | "id"
                       | "es"
                       | "se"
                       | "em"
                       | "me"
                       | "en"
                       | "ne"
                       | "it"
                       | "ti"
                       | "ni"
                       | "in"
                       | "ev"
                       | "ve"
                       | "er"
                       | "re" -> 0.
                       | _ -> dist *. v)
                    | _ -> dist *. v)
                  :: slaps
                else slaps
              in
              let scissors =
                if Hand.Infix.(k1.hand = k2.hand)
                   && (not (Float.equal slope Float.zero))
                   && (not (Float.is_nan slope))
                   && o1 <> o2
                then (
                  match preferred_rowchange_slope_dir k1.finger k2.finger with
                  | None -> scissors
                  | Some preferred_slope_dir ->
                    let preferred_slope_dir =
                      preferred_slope_dir * if Hand.equal k1.hand `L then 1 else -1
                    in
                    let slope_dir = Float.abs slope /. slope in
                    if Float.is_nan slope_dir
                    then scissors
                    else (
                      let slope_dir = slope_dir |> Float.to_int in
                      if slope_dir <> preferred_slope_dir
                      then
                        Incr.map2 freq_s1 freq_s29 ~f:(fun freq_s1 freq_s29 ->
                          (freq_s1 +. freq_s29) *. dist')
                        :: scissors
                      else scissors))
                else scissors
              in
              let lsb =
                if Hand_finger.adjacent_no_thumb hf1 hf2 && not (Float.equal dist 1.)
                then
                  Incr.map freq_s1 ~f:(fun v -> (Float.abs (dist -. 1.) +. 1.) *. v)
                  :: lsb
                else lsb
              in
              let layer_transitions =
                if k1.layer <> k2.layer
                then
                  Incr.map2 freq_s1 freq_s29 ~f:(fun f1 f29 -> f1 +. f29)
                  :: layer_transitions
                else layer_transitions
              in
              let layer_trigger_s129 =
                match k1.layer_trigger, k2.layer_trigger with
                | None, None -> layer_trigger_s129
                | None, Some k2lti ->
                  let k2lt, _incr = layout.(k2lti) in
                  if Hand_finger.equal (Key.hand_finger k1) (Key.hand_finger k2lt)
                  then (
                    let dist =
                      Float.sqrt (((k1.y -. k2lt.y) ** 2.) +. ((k1.x -. k2lt.x) ** 2.))
                    in
                    Incr.map2 freq_s1 freq_s29 ~f:(fun freq_1 freq_29 ->
                      (freq_1 +. freq_29) *. dist)
                    :: layer_trigger_s129)
                  else layer_trigger_s129
                | Some k1lti, None ->
                  let k1lt, _incr = layout.(k1lti) in
                  if Hand_finger.equal (Key.hand_finger k1lt) (Key.hand_finger k2)
                  then (
                    let dist =
                      Float.sqrt (((k1lt.y -. k2.y) ** 2.) +. ((k1lt.x -. k2.x) ** 2.))
                    in
                    Incr.map2 freq_s1 freq_s29 ~f:(fun freq_1 freq_29 ->
                      (freq_1 +. freq_29) *. dist)
                    :: layer_trigger_s129)
                  else layer_trigger_s129
                | Some k1lti, Some k2lti ->
                  let res1 =
                    let k1lt, _incr = layout.(k1lti) in
                    if Hand_finger.equal (Key.hand_finger k1lt) (Key.hand_finger k2)
                    then (
                      let dist =
                        Float.sqrt (((k1lt.y -. k2.y) ** 2.) +. ((k1lt.x -. k2.x) ** 2.))
                      in
                      Incr.map2 freq_s1 freq_s29 ~f:(fun freq_1 freq_29 ->
                        (freq_1 +. freq_29) *. dist)
                      :: [])
                    else []
                  in
                  let res2 =
                    let k2lt, _incr = layout.(k2lti) in
                    if Hand_finger.equal (Key.hand_finger k1) (Key.hand_finger k2lt)
                    then (
                      let dist =
                        Float.sqrt (((k1.y -. k2lt.y) ** 2.) +. ((k1.x -. k2lt.x) ** 2.))
                      in
                      Incr.map2 freq_s1 freq_s29 ~f:(fun freq_1 freq_29 ->
                        (freq_1 +. freq_29) *. dist)
                      :: [])
                    else []
                  in
                  res1 @ res2 @ layer_trigger_s129
              in
              let trigram_stats =
                Array.foldi
                  layout
                  ~init:trigram_stats
                  ~f:(fun k (badredirs, badtrills) (k3, var3) ->
                    let _l3, _o3 = Layout.layer_offset layout_ k in
                    let _hf3 = Key.hand_finger k3 in
                    let _slope2 = (k3.y -. k2.y) /. (k3.x -. k2.x) in
                    let _dist2 =
                      Float.sqrt (((k2.y -. k3.y) ** 2.) +. ((k2.x -. k3.x) ** 2.))
                    in
                    let _dist'2 =
                      Float.sqrt
                        (((k2.y -. k3.y) ** 2.)
                         +. ((k2.x
                              -. k3.x
                              -. (finger_dist k2.finger k3.finger
                                  |> Option.value ~default:0.))
                             ** 2.))
                    in
                    let trigram =
                      Incr.both bigram var3
                      |> Incr.map ~f:(fun ((c1, c2), v3) -> c1, c2, v3.code)
                    in
                    let freq_tri =
                      Incr.map trigram ~f:(Corpus.Lookup.freq3 ~data:corpus.triples)
                    in
                    let badredirs =
                      if Hand.equal k1.hand k2.hand
                         && Hand.equal k2.hand k3.hand
                         &&
                         match k1.finger, k2.finger, k3.finger with
                         | `P, `M, `R | `R, `P, `M | `R, `M, `P | `M, `P, `R -> true
                         | _, _, _ -> false
                      then freq_tri :: badredirs
                      else badredirs
                    in
                    let badtrills =
                      if Hand.equal k1.hand k2.hand
                         && Hand.equal k2.hand k3.hand
                         &&
                         match k1.finger, k2.finger, k3.finger with
                         | `R, `M, `R | `M, `R, `M | `R, `P, `R | `P, `R, `P -> true
                         | _, _, _ -> false
                      then freq_tri :: badtrills
                      else badtrills
                    in
                    badredirs, badtrills)
              in
              ( sfbs
              , sfss
              , speed
              , inrowlls
              , outrowlls
              , scissors
              , lsb
              , slaps
              , layer_transitions
              , layer_trigger_s129
              , trigram_stats ))
        in
        usage, bigrams_stats)
    in
    let usage = Map.map usage ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float) in
    let sfbs = Map.map sfbs ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float) in
    let sfss = Map.map sfss ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float) in
    let speed = Map.map speed ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float) in
    let inrowlls =
      Map.map inrowlls ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float)
    in
    let outrowlls =
      Map.map outrowlls ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float)
    in
    let scissors = scissors |> Array.of_list |> Incr.sum_float in
    let lsb = lsb |> Array.of_list |> Incr.sum_float in
    let slaps = slaps |> Array.of_list |> Incr.sum_float in
    let layer_transitions = layer_transitions |> Array.of_list |> Incr.sum_float in
    let layer_trigger_s129 = layer_trigger_s129 |> Array.of_list |> Incr.sum_float in
    let badredirs = badredirs |> Array.of_list |> Incr.sum_float in
    let badtrills = badtrills |> Array.of_list |> Incr.sum_float in
    { usage
    ; sfbs
    ; sfss
    ; inrowlls
    ; speed
    ; outrowlls
    ; scissors
    ; lsb
    ; slaps
    ; layer_transitions
    ; layer_trigger_s129
    ; badredirs
    ; badtrills
    }
  ;;

  let pretty_string
    { usage
    ; sfbs
    ; sfss
    ; inrowlls
    ; speed
    ; outrowlls
    ; scissors
    ; lsb
    ; slaps
    ; layer_transitions
    ; layer_trigger_s129
    ; badredirs
    ; badtrills
    }
    =
    let map stat of_alist_exn =
      stat
      |> Map.to_alist
      |> List.map ~f:(fun (k, incr) -> Incr.map incr ~f:(fun v -> k, v))
      |> Incr.all
      |> Incr.map ~f:of_alist_exn
    in
    let%map_open.Incr usage = map usage Hand_finger.Map.of_alist_exn
    and sfbs = map sfbs Hand_finger.Map.of_alist_exn
    and sfss = map sfss Hand_finger.Map.of_alist_exn
    and inrowlls = map inrowlls Hand.Map.of_alist_exn
    and speed = map speed Hand_finger.Map.of_alist_exn
    and outrowlls = map outrowlls Hand.Map.of_alist_exn
    and scissors = scissors
    and lsb = lsb
    and slaps = slaps
    and layer_transitions = layer_transitions
    and layer_trigger_s129 = layer_trigger_s129
    and badredirs = badredirs
    and badtrills = badtrills in
    let module T = Text_block in
    let table data ~to_string ~all =
      let cols =
        all
        |> List.map ~f:(fun key ->
          let data =
            data
            |> List.map ~f:snd
            |> List.map ~f:(fun map ->
              match Map.find (fst map) key with
              | None -> T.text "None"
              | Some item -> item |> Float.( * ) 100. |> sprintf "%.2f" |> T.text)
          in
          T.text (to_string key) :: data, `Right)
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
    let simple data =
      let data =
        List.map data ~f:(fun (name, total) -> name, (String.Map.empty, total))
      in
      table data ~to_string:Fn.id ~all:[]
    in
    let map_total map = Map.data map |> List.sum (module Float) ~f:Fn.id in
    let map_row name map = name, (map, map_total map) in
    let hf_table =
      table
        ~all:Hand_finger.all
        ~to_string:Hand_finger.to_string
        [ map_row "sfb" sfbs
        ; map_row "sfs" sfss
        ; map_row "speed" speed
        ; map_row "usage" usage
        ]
    in
    let hand_table =
      table
        ~all:Hand.all
        ~to_string:Hand.to_string
        [ map_row "inrowlls" inrowlls; map_row "outrowlls" outrowlls ]
    in
    let simple_table =
      simple
        [ "scissors", scissors
        ; "lsb", lsb
        ; "slaps", slaps
        ; "layer_transitions", layer_transitions
        ; "layer_trigger_s129", layer_trigger_s129
        ; "badredirs", badredirs
        ; "badtrills", badtrills
        ]
    in
    let sections =
      [ "hand-finger", hf_table; "hand", hand_table; "simple", simple_table ]
    in
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
end
