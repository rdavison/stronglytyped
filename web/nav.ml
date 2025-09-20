open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

let brute_force_indexes_button ~keyboard_inject ~keyboard_cancel ~keyboard graph =
  let%arr effects =
    let%arr keyboard_inject = keyboard_inject
    and keyboard_cancel = keyboard_cancel
    and indexes_swaps_for_brute_forcing =
      let%arr x =
        Bonsai.assoc
          (module Analysis.Hand)
          (Bonsai.Map.index_by
             keyboard
             ~comparator:(module Analysis.Hand)
             ~index:(fun (key : Analysis.Key.t) ->
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
            |> List.map ~f:(fun x ->
              print_s ([%sexp_of: Key.Id.t * Key.Id.t] x);
              x))
      in
      List.concat (Map.data x)
    in
    let effects =
      List.map indexes_swaps_for_brute_forcing ~f:(fun swap -> Keyboard.Action.Swap swap)
    in
    Ui_effect.all_unit [ keyboard_cancel; keyboard_inject effects ]
  in
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.on_click (fun _event -> effects) ]
    [ Vdom.Node.text "Brute Force indexes" ]
;;

let component
      ~keyboard
      ~keyboard_inject
      ~keyboard_cancel
      ~runtime_mode
      ~runtime_mode_vdom
      ~same_finger_controls_vdom
      ~corpus_vdom
      ~finger_dexterity_vdom
      graph
  =
  let brute_force_indexes_button =
    brute_force_indexes_button ~keyboard ~keyboard_inject ~keyboard_cancel graph
  in
  let random_swap_vdom =
    let%arr keyboard_inject = keyboard_inject
    and runtime_mode = runtime_mode in
    let button name callback =
      match runtime_mode with
      | Runtime.Mode.Auto ->
        Vdom.Node.button ~attrs:[ Vdom.Attr.disabled ] [ Vdom.Node.text name ]
      | Manual ->
        Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _event -> callback ()) ]
          [ Vdom.Node.text name ]
    in
    button "Random Swap" (fun () -> keyboard_inject [ Random_swap ])
  in
  let actions_vdom =
    let%arr random_swap_vdom = random_swap_vdom
    and brute_force_indexes_button = brute_force_indexes_button in
    Vdom.Node.div
      ~attrs:
        [ Design.Card.attr
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
      ]
  in
  let%arr same_finger_controls_vdom = same_finger_controls_vdom
  and actions_vdom = actions_vdom
  and runtime_mode_vdom = runtime_mode_vdom
  and finger_dexterity_vdom = finger_dexterity_vdom
  and corpus_vdom = corpus_vdom in
  Vdom.Node.create
    "nav"
    ~attrs:
      [ [%css
          {|
          display: flex;
          flex-direction: column;
          background-color: %{Tailwind_v3_colors.slate600#Css_gen.Color};
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
    [ corpus_vdom
    ; same_finger_controls_vdom
    ; runtime_mode_vdom
    ; actions_vdom
    ; finger_dexterity_vdom
    ]
;;
