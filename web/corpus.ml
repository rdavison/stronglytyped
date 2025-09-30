open! Import
include Stem.Corpus

module Select = struct
  module T = struct
    type t =
      | Fast
      | Custom
    [@@deriving sexp, equal, compare, enumerate]
  end

  include T

  let default = Fast

  let component ~theme graph =
    let dropdown =
      Form.Elements.Dropdown.enumerable
        ~init:(`This (Bonsai.return default))
        (module T)
        graph
    in
    let textarea =
      let extra_attrs =
        let%map dropdown = dropdown in
        match Form.value_or_default dropdown ~default with
        | Fast -> [ Vdom.Attr.disabled ]
        | Custom -> []
      in
      Form.Elements.Textarea.string ~extra_attrs () graph
    in
    let corpus =
      let%arr dropdown = dropdown
      and textarea = textarea in
      let dropdown = Form.value_or_default dropdown ~default in
      let textarea = Form.value_or_default textarea ~default:"" in
      match dropdown with
      | Fast -> fast
      | Custom -> of_string textarea
    in
    let view =
      let%arr dropdown = dropdown
      and textarea = textarea
      and theme = theme in
      let textarea_wrapper =
        match Form.value_or_default dropdown ~default with
        | Fast -> []
        | Custom -> [ Form.view textarea ]
      in
      Vdom.Node.div
        ~attrs:
          [ Design.Card.attr theme
          ; [%css
              {|
              display: flex;
              flex-direction: column;
              gap: 2px;
            |}]
          ]
        ([ Vdom.Node.label [ Vdom.Node.text "Select Corpus" ]; Form.view dropdown ]
         @ textarea_wrapper)
    in
    corpus, view
  ;;
end

(* let component graph = *)
(*   let corpus = *)
(*     let%arr dropdown = dropdown *)
(*     and textarea = textarea in *)
(*     match dropdown with *)
(*     | Custom -> of_string textarea *)
(*     | Foo -> foo *)
(*   in *)
(*   let view = *)
(*     let%arr dropdown_vdom = dropdown_vdom *)
(*     and textarea_vdom = textarea_vdom in *)
(*     Vdom.Node.div [ dropdown_vdom; textarea_vdom ] *)
(*   in *)
(*   corpus, view *)
(* ;; *)
