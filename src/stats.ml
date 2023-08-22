open! Import

type t =
  { sfbs : float Incr.t Hand_finger.Map.t
  ; sfss : float Incr.t Hand_finger.Map.t
  ; speed : float Incr.t Hand_finger.Map.t
  ; inrowlls : float Incr.t Hand.Map.t
  ; outrowlls : float Incr.t Hand.Map.t
  }
[@@deriving sexp_of]

let make (layout : Layout.t) (corpus : Corpus.t) =
  let layout = Array.map layout.keys ~f:(fun (key, var) -> key, Incr.Var.watch var) in
  let sfbs, sfss, speed, inrowlls, outrowlls =
    let init =
      let sfbs = Hand_finger.Map.empty in
      let sfss = Hand_finger.Map.empty in
      let speed = Hand_finger.Map.empty in
      let inrowlls = Hand.Map.empty in
      let outrowlls = Hand.Map.empty in
      sfbs, sfss, speed, inrowlls, outrowlls
    in
    Array.fold layout ~init ~f:(fun init (k1, var1) ->
      let hf1 = Key.hand_finger k1 in
      Array.fold
        layout
        ~init
        ~f:(fun (sfbs, sfss, speed, inrowlls, outrowlls) (k2, var2) ->
          let hf2 = Key.hand_finger k2 in
          let dist = Float.sqrt (((k1.y -. k2.y) ** 2.) +. ((k1.x -. k2.x) ** 2.)) in
          let bigram =
            Incr.both var1 var2 |> Incr.map ~f:(fun (v1, v2) -> v1.code, v2.code)
          in
          let freq_s1 = Incr.map bigram ~f:(Corpus.Lookup.freq2 ~data:corpus.s1) in
          let freq_s29 =
            let freq_s2 = Incr.map bigram ~f:(Corpus.Lookup.freq2 ~data:corpus.s2) in
            let freq_s3 = Incr.map bigram ~f:(Corpus.Lookup.freq2 ~data:corpus.s3) in
            let freq_s4 = Incr.map bigram ~f:(Corpus.Lookup.freq2 ~data:corpus.s4) in
            let freq_s5 = Incr.map bigram ~f:(Corpus.Lookup.freq2 ~data:corpus.s5) in
            let freq_s6 = Incr.map bigram ~f:(Corpus.Lookup.freq2 ~data:corpus.s6) in
            let freq_s7 = Incr.map bigram ~f:(Corpus.Lookup.freq2 ~data:corpus.s7) in
            let freq_s8 = Incr.map bigram ~f:(Corpus.Lookup.freq2 ~data:corpus.s8) in
            let freq_s9 = Incr.map bigram ~f:(Corpus.Lookup.freq2 ~data:corpus.s9) in
            [| freq_s2; freq_s3; freq_s4; freq_s5; freq_s6; freq_s7; freq_s8; freq_s9 |]
            |> Array.mapi ~f:(fun i sn ->
              Incr.map sn ~f:(fun sn -> sn /. Float.of_int (i + 1)))
            |> Incr.sum_float
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
          sfbs, sfss, speed, inrowlls, outrowlls))
  in
  let sfbs = Map.map sfbs ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float) in
  let sfss = Map.map sfss ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float) in
  let speed = Map.map speed ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float) in
  let inrowlls =
    Map.map inrowlls ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float)
  in
  let outrowlls =
    Map.map outrowlls ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float)
  in
  { sfbs; sfss; inrowlls; speed; outrowlls }
;;
