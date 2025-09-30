open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

module type Model = sig
  type t [@@deriving sexp_of, equal]

  val to_string : t -> string
end

let component
      (type model)
      (module Model : Model with type t = model)
      items
      ~equal
      ~(callback : ('item option -> unit Ui_effect.t) Bonsai.t)
      ~(f : model -> 'item)
      ~theme
      graph
  =
  let form =
    Form.Elements.Radio_buttons.list
      (module Model)
      ~equal:Model.equal
      ~to_string:Model.to_string
      ~layout:`Vertical
      items
      graph
  in
  let value =
    form
    >>| Form.value
    >>| function
    | Error _ -> None
    | Ok item -> Some (f item)
  in
  Bonsai.Edge.on_change ~equal:(Option.equal equal) value ~callback graph;
  let%arr form = form
  and theme = theme in
  let view = Form.view form in
  Vdom.Node.div
    ~attrs:
      [ Design.Card.attr theme
      ; [%css
          {|
            display: flex;
            gap: 0.5rem;
            flex-direction: column;

            /* Container */
            .widget-radio-buttons.radio-button-container {
              list-style: none;
              margin: 0;
              padding: 0;
              max-width: 100%;
              border: 1px solid #000;
            }

            /* Each row */
            .widget-radio-buttons.radio-button-container li {
              display: block;
              border-bottom: 1px solid #eee;
            }

            .widget-radio-buttons.radio-button-container li:last-child {
              border-bottom: none;
            }

            /* Hide the native radio */
            .widget-radio-buttons.radio-button-container .radio-button {
              display: none;
            }

            /* Label as row */
            .widget-radio-buttons.radio-button-container label {
              display: block;
              padding: 0.5rem 1rem;
              cursor: pointer;
              transition: background-color 0.2s, color 0.2s;
              background-color: #f5f5f5;
              color: #000;
            }

            /* Hover */
            .widget-radio-buttons.radio-button-container label:hover {
              background-color: #e5e5e5;
            }

            /* Selected row using :has() */
            .widget-radio-buttons.radio-button-container label:has(.radio-button:checked) {
              background-color: #007acc;
              color: #fff;
              font-weight: 600;
            }
           |}]
      ]
    [ Vdom.Node.label [ Vdom.Node.text "Best Layout History" ]; view ]
;;
