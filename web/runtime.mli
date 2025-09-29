open! Core
open! Bonsai_web

module Mode : sig
  include module type of Analysis.Runtime.Mode

  val start
    :  t Bonsai.t
    -> keyboard:Keyboard.t Bonsai.t
    -> keyboard_inject:(Keyboard.Action.t list -> unit Ui_effect.t) Bonsai.t
    -> keyboard_cancel:unit Ui_effect.t Bonsai.t
    -> set_best_layouts:((float * Analysis.Keyboard.t) list -> unit Ui_effect.t) Bonsai.t
    -> every:Time_ns.Span.t Bonsai.t
    -> Bonsai.graph
    -> unit

  module Select : sig
    val component
      :  runtime_mode_inject:(t -> unit Ui_effect.t) Bonsai.t
      -> Bonsai.graph
      -> t Bonsai.t * Vdom.Node.t Bonsai.t
  end
end
