open! Bonsai_web
module Corpus = Stronglytyped_analysis.Corpus
module Key = Stronglytyped_analysis.Key

val component : (Corpus.t * Vdom.Node.t) Computation.t
