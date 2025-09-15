open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open Analysis.Counter

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
        ([ [%css
             {|
                display: flex;
                justify-content: center;
                align-items: center;
                width: 2ch;
                border: 1px black solid;
                background-color: #334;
                cursor: default;

                &:hover {
                  background-color: #445;
                }

                &:active {
                  background-color: #223;
                  color: white;
                }
            |}]
         ; Vdom.Attr.on_click (fun _event -> inject action)
         ]
         @ disabled)
      [ Vdom.Node.text label ]
  in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
            display: flex;
            justify-content: space-evenly;
            user-select: none;
          |}]
      ]
    [ button "-" Decrement
    ; Vdom.Node.div ~attrs:[] [ Vdom.Node.text (msg n) ]
    ; button "+" Increment
    ]
;;
