open! Core
open! Bonsai_web

include
  Analysis.Corpus_intf.S
  with module Maps = Analysis.Corpus.Maps
   and type t = Analysis.Corpus.t

module Select : sig
  val component
    :  theme:[ `Dark | `Light ] Bonsai.t
    -> Bonsai.graph
    -> t Bonsai.t * Vdom.Node.t Bonsai.t
end
