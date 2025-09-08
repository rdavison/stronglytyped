open! Core
open! Bonsai

type info =
  { bigram : string
  ; dist : float
  ; freqs : float Corpus.Bigrams.t
  }

type t = info Key.Id.Pair.Map.t

val make : Keyboard.t Bonsai.t -> Corpus.t Bonsai.t -> Bonsai.graph -> t Bonsai.t
