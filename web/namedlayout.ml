open! Import
include Stem.Namedlayout

module Select = struct
  module T = struct
    type nonrec t =
      | QWERTY
      | Colemak
      | Dvorak
    [@@deriving sexp, equal, compare, enumerate]
  end

  include T

  let default = QWERTY

  let component ~keyboard_inject graph =
    let dropdown =
      Form.Elements.Dropdown.enumerable
        ~init:(`This (Bonsai.return default))
        (module T)
        graph
    in
    let data =
      let%arr dropdown = dropdown in
      Form.value_or_default dropdown ~default
    in
    let () =
      Bonsai.Edge.on_change
        data
        ~equal:T.equal
        ~callback:
          (let%arr keyboard_inject = keyboard_inject in
           fun (t : t) ->
             keyboard_inject
               [ Keyboard.Action.Overwrite
                   (match t with
                    | QWERTY -> qwerty
                    | Colemak -> colemak
                    | Dvorak -> dvorak)
               ])
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
        [ Vdom.Node.label [ Vdom.Node.text "Set Layout" ]; Form.view dropdown ]
    in
    data, view
  ;;
end

module With_score = struct
  type t =
    { name : string option
    ; score : float option
    ; keyboard : Stem.Keyboard.t
    }
  [@@deriving sexp, equal]

  let to_string (t : t) =
    match t.name with
    | Some name -> name
    | None ->
      (match t.score with
       | Some score -> sprintf "%.2f" score
       | None -> "???")
  ;;
end
