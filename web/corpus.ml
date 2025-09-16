open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view
include Analysis.Corpus

module T = struct
  type t =
    | Foo
    | Custom
  [@@deriving sexp, equal, compare, enumerate]
end

let component graph =
  let dropdown, dropdown_vdom =
    let form = Form.Elements.Dropdown.enumerable (module T) graph in
    let value =
      let%arr form = form in
      form |> Form.value_or_default ~default:T.Foo
    in
    let vdom =
      let%arr form = form in
      Form.view form
    in
    value, vdom
  in
  let textarea, textarea_vdom =
    let extra_attrs =
      let%map dropdown = dropdown in
      match dropdown with
      | Foo -> [ Vdom.Attr.disabled ]
      | Custom -> []
    in
    let form = Form.Elements.Textarea.string ~extra_attrs () graph in
    let value =
      let%arr form = form in
      Form.value_or_default ~default:"" form
    in
    let vdom =
      let%arr form = form in
      Form.view form
    in
    value, vdom
  in
  let corpus =
    let%arr dropdown = dropdown
    and textarea = textarea in
    match dropdown with
    | Custom -> of_string textarea
    | Foo -> foo
  in
  let view =
    let%arr dropdown_vdom = dropdown_vdom
    and textarea_vdom = textarea_vdom in
    Vdom.Node.div [ dropdown_vdom; textarea_vdom ]
  in
  corpus, view
;;
