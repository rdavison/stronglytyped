open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Key = Stronglytyped_analysis.Key
module Keyboard = Stronglytyped_analysis.Keyboard
module Corpus = Stronglytyped_analysis.Corpus

type info =
  { corpus_key : string
  ; dist : float
  ; freqs : float Corpus.bigrams
  }

type t = info Key.Id.Pair.Map.t

let make (keyboard : Keyboard.t Bonsai.t) corpus graph =
  let id_id_map =
    Bonsai.assoc
      (module Key.Id)
      keyboard
      graph
      ~f:(fun _ v1 graph ->
        let foo =
          Bonsai.assoc
            (module Key.Id)
            keyboard
            graph
            ~f:(fun _ v2 _graph ->
              let%arr v1 = v1
              and v2 = v2
              and corpus = corpus in
              let corpus_key = Key.bigram v1 v2 in
              let dist = Key.dist v1 v2 in
              { corpus_key; dist; freqs = Corpus.bigrams corpus corpus_key })
        in
        foo)
  in
  Bonsai.Map.collapse id_id_map ~comparator:(module Key.Id) graph
;;
