open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let vdom score = Vdom.Node.textf "%.2f" score

let component ~same_finger_stats _graph =
  let score = Analysis.Score.score ~same_finger_stats in
  let vdom =
    let%arr score = score in
    match score with
    | None -> Vdom.Node.text "???"
    | Some score -> vdom score
  in
  score, vdom
;;
