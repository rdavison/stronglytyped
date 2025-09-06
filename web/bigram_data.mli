open! Core
open! Bonsai_web
module Key := Stronglytyped_analysis.Key
module Keyboard := Stronglytyped_analysis.Keyboard
module Corpus := Stronglytyped_analysis.Corpus

type info =
  { corpus_key : string
  ; dist : float
  ; freqs : float Corpus.bigrams
  }

type t = info Key.Id.Pair.Map.t

val make : Keyboard.t Bonsai.t -> Corpus.t Bonsai.t -> Bonsai.graph -> t Bonsai.t
