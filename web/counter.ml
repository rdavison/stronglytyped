open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open Stronglytyped_analysis.Counter

let vdom ~n ~inject ~msg =
  let%arr n = n
  and inject = inject in
  let button label (action : Action.t) =
    let disabled =
      match action with
      | Decrement when n <= 0 -> [ Vdom.Attr.disabled ]
      | _ -> []
    in
    Vdom.Node.div
      ~attrs:
        ([ Style.counter_button; Vdom.Attr.on_click (fun _event -> inject action) ]
         @ disabled)
      [ Vdom.Node.text label ]
  in
  Vdom.Node.div
    ~attrs:[ Style.counter_container ]
    [ button "-" Decrement
    ; Vdom.Node.div ~attrs:[ Style.counter ] [ Vdom.Node.text (msg n) ]
    ; button "+" Increment
    ]
;;
