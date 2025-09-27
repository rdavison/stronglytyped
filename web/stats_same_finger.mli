open! Core
open! Bonsai_web
module Form := Bonsai_web_ui_form.With_manual_view

include
  Analysis.Stats_same_finger_intf.S
  with type ('a, 'b) metric = ('a, 'b) Analysis.Stats_same_finger.metric
   and type t = Analysis.Stats_same_finger.t
   and module Typed_variant = Analysis.Stats_same_finger.Typed_variant

val selected_metrics : ('a list, 'b) Form.t Bonsai.t -> 'a list Bonsai.t

val component
  :  keyboard:Keyboard.t Bonsai.t
  -> finger_dexterity:(Analysis.Hand_finger.t -> float) Bonsai.t
  -> corpus:Corpus.t Bonsai.t
  -> theme:[ `Dark | `Light ] Bonsai.t
  -> Bonsai.graph
  -> (Typed_variant.Packed.t, t, Typed_variant.Packed.comparator_witness) Map.t Bonsai.t
     * ( (Typed_variant.Packed.t, Typed_variant.Packed.comparator_witness) Set.t
         , Vdom.Node.t )
         Form.t
         Bonsai.t
     * Vdom.Node.t Bonsai.t
