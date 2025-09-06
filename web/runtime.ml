open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Mode = struct
  type t =
    | Auto
    | Manual
  [@@deriving sexp, equal, compare]

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
    let vdom =
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
    mode, vdom
  ;;
end
