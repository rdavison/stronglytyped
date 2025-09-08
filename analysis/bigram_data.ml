open! Core
open! Bonsai
open! Bonsai.Let_syntax

type info =
  { bigram : string
  ; dist : float
  ; freqs : float Corpus.Bigrams.t
  }

type t = info Key.Id.Pair.Map.t

let make (keyboard : Keyboard.t Bonsai.t) corpus graph =
  let id_id_map =
    Bonsai.assoc
      (module Key.Id)
      keyboard
      graph
      ~f:(fun _id1 key1 graph ->
        Bonsai.assoc
          (module Key.Id)
          keyboard
          graph
          ~f:(fun _id2 key2 _graph ->
            let%arr key1 = key1
            and key2 = key2
            and corpus = corpus in
            let bigram = Key.bigram key1 key2 in
            let dist = Key.dist key1 key2 in
            { bigram; dist; freqs = Corpus.bigrams corpus bigram }))
  in
  Bonsai.Map.collapse id_id_map ~comparator:(module Key.Id) graph
;;
