open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Style =
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

        body {
          background-color: #94A3B8;
        }
      |}]

let app graph =
  let keyboard, keyboard_inject, keyboard_cancel = Keyboard.state_machine graph in
  let _namedlayout, namedlayout_vdom =
    Namedlayout.Select.component ~keyboard_inject graph
  in
  let corpus, corpus_vdom = Corpus.Select.component graph in
  let finger_dexterity =
    Bonsai_web_ui_form.With_manual_view.Elements.Range.float
      ~min:(Bonsai.return 1.)
      ~max:(Bonsai.return 5000.)
      ~step:(Bonsai.return 1.)
      ~default:(Bonsai.return 2000.)
      ()
      graph
  in
  let finger_dexterity_vdom =
    let%arr finger_dexterity = finger_dexterity in
    Bonsai_web_ui_form.With_manual_view.view finger_dexterity
  in
  let same_finger_stats, same_finger_controls, stats_section_vdom =
    let finger_dexterity =
      let%arr finger_dexterity = finger_dexterity in
      fun _hand_finger ->
        Bonsai_web_ui_form.With_manual_view.value_or_default
          finger_dexterity
          ~default:2000.
    in
    Stats_same_finger.component ~keyboard ~finger_dexterity ~corpus graph
  in
  let _score, score_vdom = Score.component ~same_finger_stats graph in
  let keyboard_section_vdom =
    Keyboard.section_component ~keyboard ~keyboard_inject ~corpus graph
  in
  let same_finger_controls_vdom =
    let%arr same_finger_controls = same_finger_controls in
    Bonsai_web_ui_form.With_manual_view.view same_finger_controls
  in
  let runtime_mode, runtime_mode_vdom = Runtime.Mode.component graph in
  Runtime.Mode.start
    runtime_mode
    ~f:
      (let%map keyboard_inject = keyboard_inject in
       keyboard_inject [ Keyboard.Action.Random_swap ])
    graph;
  let nav =
    Nav.component
      ~keyboard
      ~keyboard_inject
      ~keyboard_cancel
      ~namedlayout_vdom
      ~runtime_mode
      ~runtime_mode_vdom
      ~same_finger_controls_vdom
      ~corpus_vdom
      ~finger_dexterity_vdom
      graph
  in
  let%arr stats_section_vdom = stats_section_vdom
  and keyboard_section_vdom = keyboard_section_vdom
  and score_vdom = score_vdom
  and nav = nav in
  let header =
    Vdom.Node.header
      ~attrs:
        [ [%css
            {|
              z-index: -1;
              width: 100%;
              padding: 1rem;
              background-color: %{Tailwind_v3_colors.slate500#Css_gen.Color};
              color: white;
              text-shadow: 0.2rem 0.2rem 0.5rem black;
              display: flex;
              flex-direction: row-reverse;
              justify-content: space-between;

              & > h1 {
                font-size: 2rem;
                height: 2rem;
                display: flex;
                justify-content: center;
                align-items: center;
              }
            |}]
        ]
      [ Vdom.Node.h1 [ Vdom.Node.text "Stronglytyped Keyboard Layout Analysis" ] ]
  in
  let footer = Vdom.Node.footer [] in
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
      [ header
      ; Vdom.Node.div
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
      ; footer
      ]
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
            color: black;
            overflow: hidden;
          |}]
      ]
    [ nav; main ]
;;

module Foo = struct
  (* open! Async_kernel *)
  (* open! Async_js *)

  (* let wait ms = after (Time_float.Span.of_int_ms ms) *)

  (* let rec connect_and_reload_on_reconnect ~(uri : Uri.t) () : unit Deferred.t = *)
  (*   Monitor.try_with (fun () -> *)
  (*     Cohttp_async_websocket.Client.with_websocket_client *)
  (*       uri *)
  (*       ~f:(fun _resp (ws : string Websocket.t) -> *)
  (*         let read_pipe, _write_pipe = Websocket.pipes ws in *)
  (*         let closed = Ivar.create () in *)
  (*         (* consume frames just to keep the pipe drained *) *)
  (*         let rec reader () = *)
  (*           match%bind Pipe.read read_pipe with *)
  (*           | `Eof -> *)
  (*             Ivar.fill_if_empty closed (); *)
  (*             return () *)
  (*           | `Ok _ -> reader () *)
  (*         in *)
  (*         don't_wait_for (reader ()); *)
  (*         (* if we got here after a previous failure, we just reconnected → reload *) *)
  (*         Js_of_ocaml.Dom_html.window##.location##reload; *)
  (*         Ivar.read closed)) *)
  (*   >>= function *)
  (*   | Ok _ -> *)
  (*     (* socket closed normally; start over *) *)
  (*     connect_and_reload_on_reconnect ~uri () *)
  (*   | Error _exn -> *)
  (*     (* failed to connect — retry with backoff *) *)
  (*     let rec loop delay = *)
  (*       Monitor.try_with (fun () -> *)
  (*         Cohttp_async_websocket.Client.with_websocket_client uri ~f:(fun _ _ -> *)
  (*           return ())) *)
  (*       >>= function *)
  (*       | Ok _ -> *)
  (*         (* success → reload *) *)
  (*         Js_of_ocaml.Dom_html.window##.location##reload; *)
  (*         Deferred.unit *)
  (*       | Error _ -> *)
  (*         let next = Int.min 3000 (int_of_float (float delay *. 1.6) |> max 300) in *)
  (*         let%bind () = wait delay in *)
  (*         loop next *)
  (*     in *)
  (*     loop 300 *)
  (* ;; *)
end

let () = Bonsai_web.Start.start app

(* let _ = *)
(*   let open Async_kernel in *)
(*   let open Async_rpc_kernel in *)
(*   Async_js.init (); *)
(*   don't_wait_for *)
(*     (let conn = *)
(*        let r, w = Pipe.create () in *)
(*        Rpc.Connection.create *)
(*          ~connection_state:(fun _ -> ()) *)
(*          (Pipe_transport.create Pipe_transport.Kind.string r w) *)
(*      in *)
(*      match%bind conn with *)
(*      | Error exn -> *)
(*        printf "Connection failed: %s\n" (Exn.to_string exn); *)
(*        return () *)
(*      | Ok conn -> *)
(*        let%bind response = *)
(*          Rpc.Rpc.dispatch Stronglytyped_rpc.Protocol.Version.t conn () *)
(*        in *)
(*        (match response with *)
(*         | Ok s -> printf "Server replied: %s\n" s *)
(*         | Error e -> printf "Error: %s\n" (Error.to_string_hum e)); *)
(*        Rpc.Connection.close conn) *)
(* ;; *)
(**)
(* let () = *)
(*   Bonsai_web.Start.start (fun _ -> *)
(*     Bonsai.return (Vdom.Node.text (Bonsai.Debug.to_dot _app))) *)
(* ;; *)
