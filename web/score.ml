open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let component _stats ~config:_ _graph = Bonsai.return Vdom.Node.none
