open! Import
open! Stem.Counter

val vdom
  :  n:int Bonsai.t
  -> inject:(Action.t -> unit Ui_effect.t) Bonsai.t
  -> msg:(int -> string)
  -> Vdom.Node.t Bonsai.t
