open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module M = Bonsai.Effect_throttling

module Mode = struct
  include Analysis.Runtime.Mode

  let component graph =
    let mode, mode_toggle =
      Bonsai.state_machine
        ~default_model:Manual
        ~apply_action:(fun _ctx model () ->
          match model with
          | Auto -> Manual
          | Manual -> Auto)
        graph
    in
    let button =
      let%arr mode = mode
      and mode_toggle = mode_toggle in
      let button name =
        Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _event -> mode_toggle ()) ]
          [ Vdom.Node.text name ]
      in
      match mode with
      | Auto -> button "Manual"
      | Manual -> button "Auto"
    in
    let vdom =
      let%arr button = button in
      Vdom.Node.div
        ~attrs:
          [ Design.card
          ; [%css
              {|
                display: flex;
                flex-direction: column;
              |}]
          ]
        [ Vdom.Node.label [ Vdom.Node.text "Runtime Mode" ]; button ]
    in
    mode, vdom
  ;;
end
