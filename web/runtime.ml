open! Core
open! Bonsai_web_proc
open! Bonsai.Let_syntax

let _ = Bonsai_proc.state_machine

module Mode = struct
  module T = struct
    type t =
      | Auto
      | Manual
    [@@deriving sexp, equal, compare]
  end

  include T

  let component =
    let%sub mode, mode_toggle =
      Bonsai.state_machine
        ~default_model:Manual
        ~apply_action:(fun _ctx model () ->
          match model with
          | Auto -> Manual
          | Manual -> Auto)
        ()
    in
    let%arr mode = mode
    and mode_toggle = mode_toggle in
    let button name =
      Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _event -> mode_toggle ()) ]
        [ Vdom.Node.text name ]
    in
    let vdom =
      match mode with
      | Auto -> button "Manual"
      | Manual -> button "Auto"
    in
    mode, vdom
  ;;
end
