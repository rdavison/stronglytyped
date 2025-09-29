open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

module Style = struct
  include
    [%css
      stylesheet
        {|
        * {
          margin: 0;
          padding: 0;
          box-sizing: border-box;
        }

        html, body {
          height: 100%;
        }
      |}]
end

let app graph =
  let theme, theme_checkbox =
    let default = true in
    let form = Form.Elements.Checkbox.bool ~default () graph in
    let value =
      let%arr form = form in
      match Form.value_or_default form ~default with
      | false -> `Light
      | true -> `Dark
    in
    let vdom =
      let%arr form = form
      and value = value in
      Vdom.Node.div
        ~attrs:
          [ Design.Card.attr value
          ; [%css
              {|
                display: flex;
                flex-direction: row;
                gap: 1rem;
              |}]
          ]
        [ Vdom.Node.label [ Vdom.Node.text "Dark Mode" ]; Form.view form ]
    in
    value, vdom
  in
  let keyboard, keyboard_inject, keyboard_cancel = Keyboard.state_machine graph in
  let _namedlayout, namedlayout_vdom =
    Namedlayout.Select.component ~keyboard_inject graph
  in
  let runtime_mode, runtime_mode_inject = Bonsai.state `Manual graph in
  let runtime_mode_vdom =
    let _selected_form, form_vdom =
      Runtime.Mode.Select.component ~runtime_mode_inject graph
    in
    form_vdom
  in
  let corpus, corpus_vdom = Corpus.Select.component ~theme graph in
  let finger_dexterity_default = 2000. in
  let finger_dexterity =
    Form.Elements.Range.float
      ~min:(Bonsai.return 1.)
      ~max:(Bonsai.return 5000.)
      ~step:(Bonsai.return 1.)
      ~default:(Bonsai.return finger_dexterity_default)
      ()
      graph
  in
  let finger_dexterity_vdom =
    let%arr finger_dexterity = finger_dexterity in
    Form.view finger_dexterity
  in
  let config =
    let%map corpus = corpus
    and finger_dexterity = finger_dexterity in
    let finger_dexterity =
      Form.value_or_default finger_dexterity ~default:finger_dexterity_default
    in
    { Analysis.Config.corpus; finger_dexterity }
  in
  let () =
    let dispatch =
      Bonsai_web.Rpc_effect.Rpc.dispatcher Stronglytyped_rpc.Protocol.Config.set graph
    in
    Bonsai.Edge.on_change
      ~equal:Analysis.Config.equal
      config
      ~callback:
        (let%map dispatch = dispatch in
         fun config ->
           match%map.Ui_effect dispatch config with
           | Ok () -> ()
           | Error e -> Error.to_string_hum e |> print_endline)
      graph
  in
  let same_finger_stats, same_finger_controls, stats_section_vdom =
    let finger_dexterity =
      let%arr finger_dexterity = finger_dexterity in
      fun _hand_finger -> Form.value_or_default finger_dexterity ~default:2000.
    in
    Stats_same_finger.component ~keyboard ~finger_dexterity ~corpus ~theme graph
  in
  let _score, score_vdom = Score.component ~same_finger_stats graph in
  let keyboard_section_vdom =
    Keyboard.section_component ~keyboard ~keyboard_inject ~corpus ~theme graph
  in
  let same_finger_controls_vdom =
    let%arr same_finger_controls = same_finger_controls in
    Form.view same_finger_controls
  in
  let poll_rate, poll_rate_vdom =
    let min = 0.5 in
    let form =
      Form.Elements.Range.float
        ~min:(Bonsai.return min)
        ~max:(Bonsai.return 1.)
        ~step:(Bonsai.return 0.1)
        ~default:(Bonsai.return min)
        ()
        graph
    in
    let value =
      let%arr form = form in
      let multiplier = 1. -. Form.value_or_default form ~default:min in
      Time_ns.Span.of_ms (multiplier *. 500.)
    in
    let vdom =
      let%arr form = form in
      Form.view form
    in
    value, vdom
  in
  let best_layouts, set_best_layouts = Bonsai.state [] graph in
  let best_layout_history_vdom =
    let module Model = struct
      type t = float * Analysis.Keyboard.t [@@deriving sexp, equal]

      let to_string (t : t) =
        let score, _ = t in
        sprintf "%.2f" score
      ;;
    end
    in
    let form =
      Form.Elements.Radio_buttons.list
        (module Model)
        ~equal:Model.equal
        ~to_string:Model.to_string
        ~layout:`Vertical
        best_layouts
        graph
    in
    let value =
      form
      >>| Form.value
      >>| function
      | Error _ -> None
      | Ok (_, keyboard) -> Some keyboard
    in
    Bonsai.Edge.on_change
      ~equal:(Option.equal Keyboard.equal)
      value
      ~callback:
        (let%arr keyboard_inject = keyboard_inject
         and runtime_mode_inject = runtime_mode_inject
         and keyboard_cancel = keyboard_cancel in
         function
         | None -> Ui_effect.Ignore
         | Some keyboard ->
           let overwrite = Key.Id.Map.map keyboard ~f:(fun (key : Key.t) -> key.kc) in
           Ui_effect.all_unit
             [ runtime_mode_inject `Manual
             ; keyboard_cancel
             ; keyboard_inject [ Analysis.Keyboard.Action.Overwrite overwrite ]
             ])
      graph;
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
  in
  Runtime.Mode.start
    runtime_mode
    ~keyboard
    ~keyboard_inject
    ~keyboard_cancel
    ~set_best_layouts
    ~every:poll_rate
    graph;
  let nav =
    let brute_force_indexes_button =
      let%arr effects =
        Actions.brute_force_indexes ~keyboard_inject ~keyboard_cancel ~keyboard graph
      in
      Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _event -> effects) ]
        [ Vdom.Node.text "Brute Force indexes" ]
    in
    let random_swap_vdom =
      let%arr keyboard_inject = keyboard_inject
      and runtime_mode = runtime_mode in
      let name = "Random Swap" in
      match (runtime_mode : Runtime.Mode.t) with
      | `Auto | `Optimize ->
        Vdom.Node.button ~attrs:[ Vdom.Attr.disabled ] [ Vdom.Node.text name ]
      | `Manual ->
        Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _event -> keyboard_inject [ Random_swap ]) ]
          [ Vdom.Node.text name ]
    in
    let actions_vdom =
      let%arr random_swap_vdom = random_swap_vdom
      and brute_force_indexes_button = brute_force_indexes_button
      and namedlayout_vdom = namedlayout_vdom
      and theme = theme in
      Vdom.Node.div
        ~attrs:
          [ Design.Card.attr theme
          ; [%css
              {|
              display: flex;
              flex-direction: column;
              gap: 0.5rem;
            |}]
          ]
        [ Vdom.Node.label [ Vdom.Node.text "Actions" ]
        ; random_swap_vdom
        ; brute_force_indexes_button
        ; namedlayout_vdom
        ]
    in
    let%arr same_finger_controls_vdom = same_finger_controls_vdom
    and best_layout_history_vdom = best_layout_history_vdom
    and actions_vdom = actions_vdom
    and runtime_mode_vdom = runtime_mode_vdom
    and finger_dexterity_vdom = finger_dexterity_vdom
    and corpus_vdom = corpus_vdom
    and poll_rate_vdom = poll_rate_vdom
    and theme = theme
    and theme_checkbox = theme_checkbox in
    let logo =
      Vdom.Node.h1
        ~attrs:
          [ [%css
              {|
                color: white;
                padding: 0.5rem;
                text-shadow: 0.1rem 0.1rem 2px black;
              |}]
          ]
        [ Vdom.Node.text "stronglytyped" ]
    in
    let background_color = Design.accent theme in
    Vdom.Node.create
      "nav"
      ~attrs:
        [ [%css
            {|
          display: flex;
          flex-direction: column;
          background-color: %{background_color#Css_gen.Color};
          width: 20rem;
          justify-content: flex-start;
          gap: 2rem;
          padding: 2rem;
          align-items: center;
          box-shadow: 2px 0px 10px black;
          overflow: auto;
          overscroll-behavior: contain;
      |}]
        ]
      [ logo
      ; theme_checkbox
      ; best_layout_history_vdom
      ; corpus_vdom
      ; same_finger_controls_vdom
      ; runtime_mode_vdom
      ; actions_vdom
      ; finger_dexterity_vdom
      ; poll_rate_vdom
      ]
  in
  let%arr stats_section_vdom = stats_section_vdom
  and keyboard_section_vdom = keyboard_section_vdom
  and score_vdom = score_vdom
  and theme = theme
  and nav = nav in
  let main =
    let sections =
      [ Vdom.Node.div
          ~attrs:
            [ [%css
                {|
                  display: flex;
                  flex-direction: row;
                  align-items: center;
                |}]
            ]
          [ Vdom.Node.h1
              ~attrs:
                [ [%css
                    {|
                      flex: 1;
                      text-align: right;
                    |}]
                ]
              [ Vdom.Node.text "Score:" ]
          ; Vdom.Node.h1
              ~attrs:
                [ [%css
                    {|
                      flex: 1;
                      text-align: left;
                      padding-left: 0.5rem;
                    |}]
                ]
              [ score_vdom ]
          ]
      ; keyboard_section_vdom
      ; stats_section_vdom
      ]
      |> List.map ~f:(fun vdom ->
        Vdom.Node.section
          ~attrs:
            [ [%css
                {|
                  width: 100%;
                |}]
            ]
          [ vdom ])
    in
    Vdom.Node.main
      ~attrs:
        [ [%css
            {|
              display: flex;
              flex-direction: column;
              width: 100%;
              overflow: auto;
              overscroll-behavior: contain;
              align-items: center;
            |}]
        ]
      [ Vdom.Node.div
          ~attrs:
            [ [%css
                {|
                  display: flex;
                  flex-direction: column;
                  padding: 2rem;
                  gap: 2rem;
                  align-items: flex-start;
                |}]
            ]
          sections
      ]
  in
  let background_color =
    match theme with
    | `Dark -> Design.dark
    | `Light -> Design.light
  in
  let color =
    match theme with
    | `Dark -> Design.light
    | `Light -> Design.dark
  in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
            display: flex;
            flex-direction: row;
            min-width: 100%;
            height: 100%;
            font-family: monospace;
            background-color: %{background_color#Css_gen.Color};
            color: %{color#Css_gen.Color};
            overflow: hidden;
          |}]
      ]
    [ nav; main ]
;;

let _debug () =
  Bonsai_web.Start.start (fun _ ->
    Bonsai.return (Vdom.Node.text (Bonsai.Debug.to_dot app)))
;;

open! Async_kernel
open! Async_rpc_kernel

let refresh_on_version_change graph =
  let open Bonsai.Let_syntax in
  let poll_result =
    Bonsai_web.Rpc_effect.Rpc.poll
      ~equal_query:Unit.equal
      Stronglytyped_rpc.Protocol.Version.t
      ~every:(Bonsai.return (Time_ns.Span.of_ms 500.))
      (Bonsai.return ())
      graph
  in
  let version, set_version = Bonsai.state `Init graph in
  let eff =
    let%arr version = version
    and set_version = set_version
    and poll_result = poll_result in
    match version with
    | `Init ->
      (match poll_result.last_ok_response with
       | None -> Ui_effect.Ignore
       | Some ((), version) -> set_version (`Version version))
    | `Version curr ->
      (match poll_result.last_ok_response with
       | None -> Ui_effect.Ignore
       | Some ((), version) ->
         if String.equal version curr
         then Ui_effect.Ignore
         else Bonsai_web.Effect.reload_page)
    | `Term -> Bonsai_web.Effect.reload_page
  in
  Bonsai.Edge.after_display eff graph
;;

let async_main () =
  Async_js.init ();
  let () =
    Bonsai_web.Start.start (fun graph ->
      refresh_on_version_change graph;
      app graph)
  in
  return ()
;;

let _ = don't_wait_for (async_main ())
