open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let brute_force_indexes_button ~keyboard_inject ~keyboard graph =
  let%arr effects =
    let%arr keyboard_inject = keyboard_inject
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
        keyboard_inject (Analysis.Keyboard.Action.Swap swap))
    in
    Ui_effect.all_unit effects
  in
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.on_click (fun _event -> effects) ]
    [ Vdom.Node.text "Brute Force indexes" ]
;;

let component
      ~same_finger_controls
      ~keyboard
      ~keyboard_inject
      ~worst_counter_vdom
      ~corpus_vdom
      graph
  =
  let runtime_mode, runtime_mode_vdom = Runtime.Mode.component graph in
  let () =
    (ignore : unit Bonsai.t -> unit)
    @@ match%sub runtime_mode with
       | Manual -> Bonsai.return ()
       | Auto ->
         Bonsai.Edge.lifecycle
           ~after_display:
             (let%map keyboard_inject = keyboard_inject in
              keyboard_inject Analysis.Keyboard.Action.Random_swap)
           graph;
         Bonsai.return ()
  in
  let brute_force_indexes_button =
    brute_force_indexes_button ~keyboard ~keyboard_inject graph
  in
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
  let%arr worst_counter_vdom = worst_counter_vdom
  and random_swap_vdom = random_swap_vdom
  and runtime_mode_vdom = runtime_mode_vdom
  and brute_force_indexes_button = brute_force_indexes_button
  and same_finger_controls = same_finger_controls
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
          justify-content: center;
          align-items: center;
          box-shadow: 2px 0px 10px black;
      |}]
      ]
    [ Bonsai_web_ui_form.With_manual_view.view same_finger_controls
    ; runtime_mode_vdom
    ; random_swap_vdom
    ; worst_counter_vdom
    ; brute_force_indexes_button
    ; corpus_vdom
    ]
;;
