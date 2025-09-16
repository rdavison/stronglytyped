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
  let keyboard, keyboard_inject = Keyboard.state_machine graph in
  let corpus, corpus_vdom = Corpus.Select.component graph in
  let same_finger_controls, stats_section_vdom =
    Stats_same_finger.component ~keyboard ~corpus graph
  in
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
       keyboard_inject Keyboard.Action.Random_swap)
    graph;
  Runtime.Mode.start
    runtime_mode
    ~f:
      (let%map keyboard_inject = keyboard_inject in
       keyboard_inject Keyboard.Action.Random_swap)
    graph;
  let nav =
    Nav.component
      ~keyboard
      ~keyboard_inject
      ~runtime_mode
      ~runtime_mode_vdom
      ~same_finger_controls_vdom
      ~corpus_vdom
      graph
  in
  let%arr stats_section_vdom = stats_section_vdom
  and keyboard_section_vdom = keyboard_section_vdom
  and nav = nav in
  let header =
    Vdom.Node.header
      ~attrs:
        [ [%css
            {|
              z-index: -1;
              padding: 1rem;
              background-color: %{Tailwind_v3_colors.slate500#Css_gen.Color};
              color: white;
              text-shadow: 0.2rem 0.2rem 0.5rem black;
              min-height: 1rem;
              display: flex;
              flex-direction: row-reverse;
              justify-content: space-between;

              & > h1 {
                font-size: 2rem;
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
      [ keyboard_section_vdom; stats_section_vdom ]
      |> List.map ~f:(fun vdom -> Vdom.Node.section [ vdom ])
    in
    Vdom.Node.main
      ~attrs:
        [ [%css
            {|
              display: flex;
              flex-direction: column;
              width: 100%;
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
