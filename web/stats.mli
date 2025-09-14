open! Core
open! Bonsai_web

val component
  :  same_finger_vdom:Vdom.Node.t Bonsai.t
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t
