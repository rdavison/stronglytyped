open! Import
include Stem.Optimizer

module Select = struct
  let default = Random

  let component ~optimizer_inject graph =
    let dropdown =
      Form.Elements.Dropdown.enumerable
        ~init:(`This (Bonsai.return default))
        (module Stem.Optimizer)
        graph
    in
    let data =
      let%arr dropdown = dropdown in
      Form.value_or_default dropdown ~default
    in
    let () =
      Bonsai.Edge.on_change
        data
        ~equal
        ~callback:
          (let%arr optimizer_inject = optimizer_inject in
           fun (t : t) -> optimizer_inject t)
        graph
    in
    let view =
      let%arr dropdown = dropdown in
      Vdom.Node.div
        ~attrs:
          [ [%css
              {|
              display: flex;
              flex-direction: column;
              gap: 2px;
            |}]
          ]
        [ Vdom.Node.label [ Vdom.Node.text "Set Runtime Mode" ]; Form.view dropdown ]
    in
    data, view
  ;;
end

