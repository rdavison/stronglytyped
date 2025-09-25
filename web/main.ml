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
    let default = false in
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
  let corpus, corpus_vdom = Corpus.Select.component ~theme graph in
  let finger_dexterity =
    Form.Elements.Range.float
      ~min:(Bonsai.return 1.)
      ~max:(Bonsai.return 5000.)
      ~step:(Bonsai.return 1.)
      ~default:(Bonsai.return 2000.)
      ()
      graph
  in
  let finger_dexterity_vdom =
    let%arr finger_dexterity = finger_dexterity in
    Form.view finger_dexterity
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
  let runtime_mode, runtime_mode_vdom = Runtime.Mode.component ~theme graph in
  Runtime.Mode.start
    runtime_mode
    ~f:
      (let%map keyboard_inject = keyboard_inject in
       keyboard_inject [ Keyboard.Action.Random_swap ])
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
      | Auto -> Vdom.Node.button ~attrs:[ Vdom.Attr.disabled ] [ Vdom.Node.text name ]
      | Manual ->
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
    and actions_vdom = actions_vdom
    and runtime_mode_vdom = runtime_mode_vdom
    and finger_dexterity_vdom = finger_dexterity_vdom
    and corpus_vdom = corpus_vdom
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
      ; corpus_vdom
      ; same_finger_controls_vdom
      ; runtime_mode_vdom
      ; actions_vdom
      ; finger_dexterity_vdom
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
