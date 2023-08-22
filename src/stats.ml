open! Import

type t =
  { sfbs : float Incr.t Hand_finger.Map.t
  ; sfss : float Incr.t Hand_finger.Map.t
  ; inrowlls : float Incr.t Hand.Map.t
  ; outrowlls : float Incr.t Hand.Map.t
  }
[@@deriving sexp_of]

let make (layout : Layout.t) (corpus : Corpus.t) =
  let layout = Array.map layout.keys ~f:(fun (key, var) -> key, Incr.Var.watch var) in
  let sfbs, sfss, inrowlls, outrowlls =
    let init =
      let sfbs = Hand_finger.Map.empty in
      let sfss = Hand_finger.Map.empty in
      let inrowlls = Hand.Map.empty in
      let outrowlls = Hand.Map.empty in
      sfbs, sfss, inrowlls, outrowlls
    in
    Array.fold layout ~init ~f:(fun init (k1, var1) ->
      let hf1 = Key.hand_finger k1 in
      Array.fold layout ~init ~f:(fun (sfbs, sfss, inrowlls, outrowlls) (k2, var2) ->
        let hf2 = Key.hand_finger k2 in
        let bigram =
          Incr.both var1 var2 |> Incr.map ~f:(fun (v1, v2) -> v1.code, v2.code)
        in
        let freq_s1 = Incr.map bigram ~f:(Corpus.Lookup.freq2 ~data:corpus.s1) in
        let freq_s2 = Incr.map bigram ~f:(Corpus.Lookup.freq2 ~data:corpus.s2) in
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
              | None -> [ freq_s2 ]
              | Some freqs -> freq_s2 :: freqs)
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
        sfbs, sfss, inrowlls, outrowlls))
  in
  let sfbs = Map.map sfbs ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float) in
  let sfss = Map.map sfss ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float) in
  let inrowlls =
    Map.map inrowlls ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float)
  in
  let outrowlls =
    Map.map outrowlls ~f:(fun lst -> lst |> Array.of_list |> Incr.sum_float)
  in
  { sfbs; sfss; inrowlls; outrowlls }
;;
