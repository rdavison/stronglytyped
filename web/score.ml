open! Core
open! Bonsai_web_proc
open! Bonsai.Let_syntax

let component _stats ~config:_ = Computation.return Vdom.Node.none
