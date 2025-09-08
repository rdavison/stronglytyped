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
      ~f:(fun _ v1 graph ->
        Bonsai.assoc
          (module Key.Id)
          keyboard
          graph
          ~f:(fun _ v2 _graph ->
            let%arr v1 = v1
            and v2 = v2
            and corpus = corpus in
            let bigram = Key.bigram v1 v2 in
            let dist = Key.dist v1 v2 in
            { bigram; dist; freqs = Corpus.bigrams corpus bigram }))
  in
  Bonsai.Map.collapse id_id_map ~comparator:(module Key.Id) graph
;;
