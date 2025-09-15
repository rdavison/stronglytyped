open! Core
open! Bonsai_web

include
  Analysis.Corpus_intf.S
  with module Maps = Analysis.Corpus.Maps
   and type t = Analysis.Corpus.t

val component : Bonsai.graph -> t Bonsai.t * Vdom.Node.t Bonsai.t
