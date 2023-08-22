open! Import

type t =
  { usage : float Incr.t Hand_finger.Map.t
  ; sfbs : float Incr.t Hand_finger.Map.t
  ; sfss : float Incr.t Hand_finger.Map.t
  ; speed : float Incr.t Hand_finger.Map.t
  ; inrowlls : float Incr.t Hand.Map.t
  ; outrowlls : float Incr.t Hand.Map.t
  }
[@@deriving sexp_of]

let s29 = Hashtbl.create (module String)

let make (layout : Layout.t) (corpus : Corpus.t) =
  let layout = Array.map layout.keys ~f:(fun (key, var) -> key, Incr.Var.watch var) in
  let usage, (sfbs, sfss, speed, inrowlls, outrowlls) =
    let init =
      let usage = Hand_finger.Map.empty in
      let sfbs = Hand_finger.Map.empty in
      let sfss = Hand_finger.Map.empty in
      let speed = Hand_finger.Map.empty in
      let inrowlls = Hand.Map.empty in
      let outrowlls = Hand.Map.empty in
      usage, (sfbs, sfss, speed, inrowlls, outrowlls)
    in
    Array.fold layout ~init ~f:(fun (usage, bigrams_stats) (k1, var1) ->
      let hf1 = Key.hand_finger k1 in
      let monogram = Incr.map var1 ~f:(fun v1 -> v1.code) in
      let freq_mono = Incr.map monogram ~f:(Corpus.Lookup.freq1 ~data:corpus.singles) in
      let usage =
        Map.update usage hf1 ~f:(function
          | None -> [ freq_mono ]
          | Some freq -> freq_mono :: freq)
      in
      let bigrams_stats =
        Array.fold
          layout
          ~init:bigrams_stats
          ~f:(fun (sfbs, sfss, speed, inrowlls, outrowlls) (k2, var2) ->
            let hf2 = Key.hand_finger k2 in
            let dist = Float.sqrt (((k1.y -. k2.y) ** 2.) +. ((k1.x -. k2.x) ** 2.)) in
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
                    acc +. (sn /. Float.of_int (i + 1)))))
            in
            let sfbs =
              if Hand_finger.Infix.(hf1 = hf2)
              then
                Map.update sfbs hf1 ~f:(function
                  | None -> [ freq_s1 ]
                  | Some freqs -> freq_s1 :: freqs)
              else sfbs
            in
            let sfss =
              if Hand_finger.Infix.(hf1 = hf2)
              then
                Map.update sfss hf1 ~f:(function
                  | None -> [ freq_s29 ]
                  | Some freqs -> freq_s29 :: freqs)
              else sfss
            in
            let speed =
              if Hand_finger.Infix.(hf1 = hf2)
              then (
                let speed' =
                  Incr.map2 freq_s1 freq_s29 ~f:(fun freq_s1 freq_s29 ->
                    dist *. (freq_s1 +. (freq_s29 /. 2.)))
                in
                Map.update speed hf1 ~f:(function
                  | None -> [ speed' ]
                  | Some speeds -> speed' :: speeds))
              else sfss
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
            sfbs, sfss, speed, inrowlls, outrowlls)
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
  { usage; sfbs; sfss; inrowlls; speed; outrowlls }
;;
