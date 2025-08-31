open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

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
      Bonsai.state_machine0
        (module T)
        (module Unit)
        ~default_model:Manual
        ~apply_action:(fun ~inject:_ ~schedule_event:_ model () ->
          match model with
          | Auto -> Manual
          | Manual -> Auto)
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
