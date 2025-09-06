open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Key = Stronglytyped_analysis.Key
module Hand_finger = Stronglytyped_analysis.Hand_finger

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

let app graph =
  let runtime_mode, runtime_mode_vdom = Runtime.Mode.component graph in
  let keyboard, keyboard_inject = Keyboard.state_machine graph in
  let brute_force_indexes_button =
    let%arr effects =
      let%arr keyboard_inject = keyboard_inject
      and indexes_swaps_for_brute_forcing =
        let%arr x =
          Bonsai.assoc
            (module Stronglytyped_analysis.Hand)
            (Bonsai.Map.index_by
               keyboard
               ~comparator:(module Stronglytyped_analysis.Hand)
               ~index:(fun key ->
                 if Stronglytyped_analysis.Finger.equal key.finger `i
                 then Some key.hand
                 else None)
               graph)
            graph
            ~f:(fun _ data graph ->
              let%arr keys = Bonsai.Map.keys data graph in
              let keys = Set.to_list keys in
              let visited = ref Stronglytyped_analysis.Key.Id.Pair.Set.empty in
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
    Stats.counter 6 (fun n -> sprintf "%d" n) graph
  in
  let corpus, corpus_vdom = Corpus.component graph in
  let stats_section_vdom = Stats.component keyboard corpus worst_counter graph in
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
  let%arr stats_section_vdom = stats_section_vdom
  and keyboard_section_vdom = keyboard_section_vdom
  and worst_counter_vdom = worst_counter_vdom
  and random_swap_vdom = random_swap_vdom
  and runtime_mode_vdom = runtime_mode_vdom
  and brute_force_indexes_button = brute_force_indexes_button
  and corpus_vdom = corpus_vdom in
  let nav =
    Vdom.Node.create
      "nav"
      ~attrs:[ with_color Style.nav ~background_color:Tailwind_v3_colors.slate600 ]
      [ runtime_mode_vdom
      ; random_swap_vdom
      ; worst_counter_vdom
      ; brute_force_indexes_button
      ]
  in
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
      [ keyboard_section_vdom; stats_section_vdom; corpus_vdom ]
      |> List.map ~f:(fun vdom -> Vdom.Node.section [ vdom ])
    in
    Vdom.Node.main
      ~attrs:[ Style.main ]
      [ header; Vdom.Node.div ~attrs:[ Style.main_body ] sections; footer ]
  in
  Vdom.Node.div ~attrs:[ Style.root ] [ nav; main ]
;;

let () = Bonsai_web.Start.start app

(* let () = *)
(*   Bonsai_web.Start.start (fun _ -> *)
(*     Bonsai.return (Vdom.Node.text (Bonsai.Debug.to_dot app))) *)
(* ;; *)
