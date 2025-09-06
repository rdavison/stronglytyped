open! Core
open! Bonsai_web
module Corpus := Stronglytyped_analysis.Corpus
module Key := Stronglytyped_analysis.Key

val component : Bonsai.graph -> (Corpus.t * Vdom.Node.t) Bonsai.t
