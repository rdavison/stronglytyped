open! Import

module Mode = struct
  include Stem.Runtime.Mode

  module Select = struct
    let default = Manual

    let component ~runtime_mode_inject graph =
      let dropdown =
        Form.Elements.Dropdown.enumerable
          ~init:(`This (Bonsai.return default))
          (module Stem.Runtime.Mode)
          graph
      in
      let data =
        let%arr dropdown = dropdown in
        Form.value_or_default dropdown ~default
      in
      let _commented_out () =
        Bonsai.Edge.on_change
          data
          ~equal:Stem.Runtime.Mode.equal
          ~callback:
            (let%arr runtime_mode_inject = runtime_mode_inject in
             fun (t : t) ->
               print_endline "Runtime mode inject";
               runtime_mode_inject t)
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

  let start
        (t : t Bonsai.t)
        ~keyboard
        ~keyboard_inject
        ~keyboard_cancel
        ~set_best_layouts
        ~every
        graph
    =
    let bonsai =
      match%sub t with
      | Manual -> Bonsai.return ()
      | Optimize_browser ->
        let eff =
          let%arr keyboard_inject = keyboard_inject in
          keyboard_inject [ Keyboard.Action.Random_swap ]
        in
        let _commented_out () = Bonsai.Edge.lifecycle ~after_display:eff graph in
        Bonsai.return ()
      | Optimize_server ->
        let poll_result =
          Bonsai_web.Rpc_effect.Rpc.poll
            ~equal_query:Unit.equal
            Stem.Protocol.Gen.t
            ~every
            (Bonsai.return ())
            graph
        in
        let keeb, set_keeb = Bonsai.state_opt graph in
        let keyboard =
          let%arr keeb = keeb
          and keyboard = keyboard in
          match keeb with
          | None -> keyboard
          | Some keyboard -> keyboard
        in
        let eff =
          let%arr keyboard = keyboard
          and set_keeb = set_keeb
          and poll_result = poll_result
          and keyboard_inject = keyboard_inject
          and keyboard_cancel = keyboard_cancel
          and set_best_layouts = set_best_layouts in
          match poll_result.last_ok_response with
          | None -> Ui_effect.Ignore
          | Some ((), (server_keeb, window)) ->
            if Stem.Keyboard.equal keyboard server_keeb
            then Ui_effect.Ignore
            else
              Ui_effect.all_unit
                [ keyboard_cancel
                ; keyboard_inject
                    [ Stem.Keyboard.Action.Overwrite
                        (Map.map server_keeb ~f:(fun key -> key.kc))
                    ]
                ; set_keeb (Some keyboard)
                ; set_best_layouts
                    (List.map window ~f:(fun (score, keyboard) ->
                       { Namedlayout.With_score.name = None
                       ; score = Some score
                       ; keyboard
                       }))
                ]
        in
        let _commented_out () = Bonsai.Edge.after_display eff graph in
        Bonsai.return ()
    in
    (ignore : unit Bonsai.t -> unit) bonsai
  ;;
end
