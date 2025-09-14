open! Core
open! Bonsai_web
module Corpus := Analysis.Corpus
module Key := Analysis.Key

val component : Bonsai.graph -> Corpus.t Bonsai.t * Vdom.Node.t Bonsai.t
