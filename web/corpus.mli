open! Import
include Stem.Corpus_intf.S with module Maps = Stem.Corpus.Maps and type t = Stem.Corpus.t

module Select : sig
  val component
    :  theme:[ `Dark | `Light ] Bonsai.t
    -> Bonsai.graph
    -> t Bonsai.t * Vdom.Node.t Bonsai.t
end
