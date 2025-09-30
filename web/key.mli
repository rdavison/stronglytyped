open! Import
include Stem.Key_intf.S with type t = Stem.Key.t and module Id = Stem.Key.Id

val component
  :  Id.t Bonsai.t
  -> keyboard:Stem.Keyboard.t Bonsai.t
  -> corpus_freq_a:float Char.Map.t Bonsai.t
  -> max_value:float Bonsai.t
  -> dnd:(Id.t, Id.t) Bonsai_web_ui_drag_and_drop.t Bonsai.t
  -> theme:[ `Dark | `Light ] Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t

val dragged_component
  :  Id.t Bonsai.t
  -> keyboard:Stem.Keyboard.t Bonsai.t
  -> corpus_freq_a:float Char.Map.t Bonsai.t
  -> max_value:float Bonsai.t
  -> dnd:(Id.t, Id.t) Bonsai_web_ui_drag_and_drop.t Bonsai.t
  -> theme:[ `Dark | `Light ] Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
