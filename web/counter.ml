open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open Analysis.Counter

module Style =
  [%css
    stylesheet
      {|
        .counter-container {
          display: flex;
          justify-content: space-evenly;
          user-select: none;
        }

        .counter {
        }

        .counter-button {
          display: flex;
          justify-content: center;
          align-items: center;
          width: 2ch;
          border: 1px black solid;
          background-color: #334;
          cursor: default;
        }

        .counter-button:hover {
          background-color: #445;
        }

        .counter-button:active {
          background-color: #223;
          color: white;
        }
|}]

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
