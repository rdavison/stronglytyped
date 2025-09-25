open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
include Analysis.Keyboard

let component ~keyboard ~corpus_freq_a ~max_value ~dnd graph ~theme =
  let key id graph =
    Key.component id ~keyboard ~corpus_freq_a ~max_value ~dnd ~theme graph
  in
  let arrangement = Analysis.Arrangement.ansi in
  let row row =
    row
    |> List.map ~f:(fun id -> key (Bonsai.return id) graph)
    |> Bonsai.all
    |> Bonsai.map
         ~f:
           (Vdom.Node.div
              ~attrs:
                [ [%css
                    {|
                      display: flex;
                      flex-direction: row;
                      font-size: 1rem;
                      justify-content: space-between;
                    |}]
                ])
  in
  let%arr keyboard_rows = arrangement |> List.map ~f:row |> Bonsai.all
  and dnd_sentinel = dnd >>| Bonsai_web_ui_drag_and_drop.sentinel ~name:"dnd-sentinel"
  and theme = theme in
  let background_color =
    match theme with
    | `Light -> Tailwind_v3_colors.slate700
    | `Dark -> Tailwind_v3_colors.slate300
  in
  Vdom.Node.div
    ~attrs:
      [ dnd_sentinel
      ; [%css
          {|
            display: flex;
            flex-direction: column;
            background-color: %{background_color#Css_gen.Color};
            padding: 0.5em;
            border-radius: 1em;
          |}]
      ]
    keyboard_rows
;;

let section_component
      ~keyboard
      ~keyboard_inject
      ~(corpus : Corpus.t Bonsai.t)
      ~theme
      graph
  =
  let corpus_freq_a = Bonsai.map corpus ~f:(fun corpus -> corpus.freq.a) in
  let max_value =
    Bonsai.Map.max_value corpus_freq_a ~comparator:(module Float) graph
    |> Bonsai.map ~f:(Option.value ~default:1.)
  in
  let dnd =
    let on_drop =
      let%arr keyboard_inject = keyboard_inject in
      fun a b -> keyboard_inject [ Action.Swap (a, b) ]
    in
    Bonsai_web_ui_drag_and_drop.create
      ~source_id:(module Key.Id)
      ~target_id:(module Key.Id)
      ~on_drop
      graph
  in
  let key id graph =
    Key.dragged_component id ~keyboard ~corpus_freq_a ~max_value ~dnd ~theme graph
  in
  let dragged_element = Bonsai_web_ui_drag_and_drop.dragged_element dnd ~f:key graph in
  let%arr component = component ~keyboard ~corpus_freq_a ~max_value ~dnd ~theme graph
  and dragged_element = dragged_element in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
            display: flex;
            flex-direction: column;
            justify-content: center;
            align-items: center;
            font-family: Helvetica;
          |}]
      ]
    [ dragged_element; component ]
;;
