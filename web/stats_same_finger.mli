open! Core
open! Bonsai_web
module Form := Bonsai_web_ui_form.With_manual_view

val selected_metrics : ('a list, 'b) Form.t Bonsai.t -> 'a list Bonsai.t

val component
  :  keyboard:Stronglytyped_analysis.Keyboard.t Bonsai.t
  -> corpus:Stronglytyped_analysis.Corpus.t Bonsai.t
  -> worst_counter:int Bonsai.t
  -> Bonsai.graph
  -> ( ( Stronglytyped_analysis.Stats_same_finger.Typed_variant.Packed.t
         , Stronglytyped_analysis.Stats_same_finger.Typed_variant.Packed
           .comparator_witness )
         Set.t
       , Vdom.Node.t )
       Bonsai_web_ui_form.With_manual_view.t
       Bonsai.t
     * Vdom.Node.t Bonsai.t
