open! Core
open! Bonsai_web
module Form := Bonsai_web_ui_form.With_manual_view
include Analysis.Stats_same_finger_intf.S

val selected_metrics : ('a list, 'b) Form.t Bonsai.t -> 'a list Bonsai.t

val component
  :  keyboard:Keyboard.t Bonsai.t
  -> corpus:Corpus.t Bonsai.t
  -> worst_counter:int Bonsai.t
  -> Bonsai.graph
  -> ( (Typed_variant.Packed.t, Typed_variant.Packed.comparator_witness) Set.t
       , Vdom.Node.t )
       Form.t
       Bonsai.t
     * Vdom.Node.t Bonsai.t
