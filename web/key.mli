open! Core
open! Bonsai_web
include Analysis.Key_intf.S with type t = Analysis.Key.t

val vdom : Id.t -> t option -> float Char.Map.t -> float -> Vdom.Node.t
