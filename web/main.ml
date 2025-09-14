open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Key = Analysis.Key
module Hand_finger = Analysis.Hand_finger

let with_color ?background_color ?color attr =
  let background_color =
    Option.map background_color ~f:(fun color -> Css_gen.background_color color)
  in
  let color = Option.map color ~f:(fun color -> Css_gen.color color) in
  let rest =
    [ background_color; color ] |> List.filter_opt |> List.map ~f:Vdom.Attr.style
  in
  Vdom.Attr.many (attr :: rest)
;;

let _app graph =
  let runtime_mode, runtime_mode_vdom = Runtime.Mode.component graph in
  let keyboard, keyboard_inject = Keyboard.state_machine graph in
  let brute_force_indexes_button =
    let%arr effects =
      let%arr keyboard_inject = keyboard_inject
      and indexes_swaps_for_brute_forcing =
        let%arr x =
          Bonsai.assoc
            (module Analysis.Hand)
            (Bonsai.Map.index_by
               keyboard
               ~comparator:(module Analysis.Hand)
               ~index:(fun key ->
                 if Analysis.Finger.equal key.finger `i then Some key.hand else None)
               graph)
            graph
            ~f:(fun _ data graph ->
              let%arr keys = Bonsai.Map.keys data graph in
              let keys = Set.to_list keys in
              let visited = ref Analysis.Key.Id.Pair.Set.empty in
              List.cartesian_product keys keys
              |> List.filter_map ~f:(fun (k1, k2) ->
                if Key.Id.equal k1 k2
                then None
                else (
                  let k12 = k1, k2 in
                  let k21 = k2, k1 in
                  let curr = !visited in
                  match Set.mem curr k12, Set.mem curr k21 with
                  | false, false ->
                    visited := Set.add !visited k12;
                    visited := Set.add !visited k21;
                    Some k12
                  | _, _ -> None))
              |> fun l ->
              List.take l 1
              |> List.map ~f:(fun x ->
                print_s ([%sexp_of: Key.Id.t * Key.Id.t] x);
                x))
        in
        List.concat (Map.data x)
      in
      let effects =
        List.map indexes_swaps_for_brute_forcing ~f:(fun swap ->
          keyboard_inject (Swap swap))
      in
      Ui_effect.all_unit effects
    in
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _event -> effects) ]
      [ Vdom.Node.text "Brute Force indexes" ]
  in
  let () =
    (ignore : unit Bonsai.t -> unit)
    @@ match%sub runtime_mode with
       | Manual -> Bonsai.return ()
       | Auto ->
         Bonsai.Edge.lifecycle
           ~after_display:
             (let%map keyboard_inject = keyboard_inject in
              keyboard_inject Random_swap)
           graph;
         Bonsai.return ()
  in
  let worst_counter, worst_counter_vdom =
    let n, inject = Analysis.Counter.counter 6 graph in
    let vdom = Counter.vdom ~n ~inject ~msg:(fun n -> sprintf "%d" n) in
    n, vdom
  in
  let corpus, corpus_vdom = Corpus.component graph in
  let same_finger_controls, stats_section_vdom =
    Stats_same_finger.component ~keyboard ~corpus ~worst_counter graph
  in
  let keyboard_section_vdom = Keyboard.component keyboard corpus graph in
  let random_swap_vdom =
    let%arr keyboard_inject = keyboard_inject
    and runtime_mode = runtime_mode in
    let button name callback =
      match runtime_mode with
      | Auto -> Vdom.Node.button ~attrs:[ Vdom.Attr.disabled ] [ Vdom.Node.text name ]
      | Manual ->
        Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _event -> callback ()) ]
          [ Vdom.Node.text name ]
    in
    button "Random Swap" (fun () -> keyboard_inject Random_swap)
  in
  let nav =
    let%arr worst_counter_vdom = worst_counter_vdom
    and random_swap_vdom = random_swap_vdom
    and runtime_mode_vdom = runtime_mode_vdom
    and brute_force_indexes_button = brute_force_indexes_button
    and same_finger_controls = same_finger_controls
    and corpus_vdom = corpus_vdom in
    Vdom.Node.create
      "nav"
      ~attrs:[ with_color Style.nav ~background_color:Tailwind_v3_colors.slate600 ]
      [ Bonsai_web_ui_form.With_manual_view.view same_finger_controls
      ; runtime_mode_vdom
      ; random_swap_vdom
      ; worst_counter_vdom
      ; brute_force_indexes_button
      ; corpus_vdom
      ]
  in
  let%arr stats_section_vdom = stats_section_vdom
  and keyboard_section_vdom = keyboard_section_vdom
  and nav = nav in
  let header =
    Vdom.Node.header
      ~attrs:[ with_color Style.header ~background_color:Tailwind_v3_colors.slate500 ]
      [ Vdom.Node.h1
          ~attrs:[ Style.header_h1 ]
          [ Vdom.Node.text "Stronglytyped Keyboard Layout Analysis" ]
      ]
  in
  let footer = Vdom.Node.footer [] in
  let main =
    let sections =
      [ keyboard_section_vdom; stats_section_vdom ]
      |> List.map ~f:(fun vdom -> Vdom.Node.section [ vdom ])
    in
    Vdom.Node.main
      ~attrs:[ Style.main ]
      [ header; Vdom.Node.div ~attrs:[ Style.main_body ] sections; footer ]
  in
  Vdom.Node.div ~attrs:[ Style.root ] [ nav; main ]
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

let () = Bonsai_web.Start.start _app

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
