open! Import

include
  Stem.Stats_same_finger_intf.S
  with type ('a, 'b) metric = ('a, 'b) Stem.Stats_same_finger.metric
   and type t = Stem.Stats_same_finger.t
   and module Typed_variant = Stem.Stats_same_finger.Typed_variant

val selected_metrics : ('a list, 'b) Form.t Bonsai.t -> 'a list Bonsai.t

val component
  :  keyboard:Keyboard.t Bonsai.t
  -> finger_dexterity:(Stem.Hand_finger.t -> float) Bonsai.t
  -> corpus:Corpus.t Bonsai.t
  -> theme:[ `Dark | `Light ] Bonsai.t
  -> Bonsai.graph
  -> (Keyboard.t
     * (Typed_variant.Packed.t, t, Typed_variant.Packed.comparator_witness) Map.t)
       Bonsai.t
     * ( (Typed_variant.Packed.t, Typed_variant.Packed.comparator_witness) Set.t
         , Vdom.Node.t )
         Form.t
         Bonsai.t
     * Vdom.Node.t Bonsai.t
