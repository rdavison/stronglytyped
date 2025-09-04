open! Core
open! Bonsai_web_proc
open! Bonsai.Let_syntax
module Arrangement = Stronglytyped_analysis.Arrangement
module Corpus = Stronglytyped_analysis.Corpus
module Key = Stronglytyped_analysis.Key
module Keycode = Stronglytyped_analysis.Keycode
module Keyboard = Stronglytyped_analysis.Keyboard

module Key_action = struct
  type t = Set of Keycode.t [@@deriving sexp, equal]
end

let key_state_machine id =
  Bonsai.state_machine
    ~default_model:(Key.make id ~x:Key.Id.Ansi.x ~y:Key.Id.Ansi.y)
    ~apply_action:(fun _ model action ->
      match action with
      | Key_action.Set kc ->
        print_s ([%sexp_of: Key.Id.t * Keycode.t] (model.id, kc));
        { model with kc })
    ()
;;

module Action = struct
  type t =
    | Swap of (Key.Id.t * Key.Id.t)
    | Random_swap
  [@@deriving sexp, compare, equal]
end

let state_machine : (Keyboard.t * (Action.t -> unit Ui_effect.t)) Computation.t =
  let ids = Key.Id.Set.of_list Key.Id.all_var in
  let%sub map =
    ids |> Key.Id.Map.of_key_set ~f:key_state_machine |> Computation.all_map
  in
  let%sub (keyboard : Keyboard.t Value.t) =
    Bonsai.assoc
      (module Key.Id)
      map
      ~f:(fun _key data ->
        let%arr key, _set_key = data in
        key)
  in
  let%arr keyboard = keyboard
  and map = map in
  let inject (action : Action.t) =
    let a, b =
      match action with
      | Random_swap -> Key.Id.rand2 ()
      | Swap ab -> ab
    in
    let a_key, a_set = Map.find_exn map a in
    let b_key, b_set = Map.find_exn map b in
    Ui_effect.all_unit [ a_set (Set b_key.kc); b_set (Set a_key.kc) ]
  in
  keyboard, inject
;;

let convert_hex_to_rgb =
  let f s = Int.of_string ("0x" ^ s) in
  fun (`Hex s) ->
    let s = String.lowercase s in
    let r = String.slice s 1 3 in
    let g = String.slice s 3 5 in
    let b = String.slice s 5 7 in
    `RGB (f r, f g, f b)
;;

let component keyboard (corpus : Corpus.t Value.t) =
  let corpus_freq_a = Value.map corpus ~f:(fun corpus -> corpus.freq.a) in
  let%sub max_value =
    Bonsai.Map.max_value corpus_freq_a ~comparator:(module Float)
    |> Computation.map ~f:(Option.value ~default:1.)
  in
  let%sub keyboard_vdom =
    let arrangement = Arrangement.ansi in
    let render_legend kc =
      match kc with
      | `Alpha _ -> Vdom.Node.text (Keycode.to_string_upper kc)
      | `Sym (_, _) ->
        Vdom.Node.div
          ~attrs:[ Style.keyboard_key_label ]
          [ Vdom.Node.div [ Vdom.Node.text (Keycode.to_string_upper kc) ]
          ; Vdom.Node.div [ Vdom.Node.text (Keycode.to_string_lower kc) ]
          ]
      | `Legend legend -> Vdom.Node.text legend
      | `Power -> Vdom.Node.text "O"
    in
    let key (id : Key.Id.t) (key : Key.t option) =
      match key with
      | None -> render_legend (Key.Id.default_kc id)
      | Some key -> render_legend key.kc
    in
    let td id =
      let%arr keyboard = keyboard
      and corpus_freq_a = corpus_freq_a
      and max_value = max_value in
      let k = Map.find keyboard id in
      match k with
      | None ->
        let keyboard_key_background_color = Tailwind_v3_colors.slate900 in
        let keyboard_key_color = Tailwind_v3_colors.slate100 in
        Vdom.Node.div
          ~attrs:
            [ Style.keyboard_key
            ; Style.Variables.set
                ~keyboard_key_background_color:
                  (keyboard_key_background_color |> Css_gen.Color.to_string_css)
                ~keyboard_key_color:(keyboard_key_color |> Css_gen.Color.to_string_css)
                ~keyboard_key_hover_background_color:
                  (Tailwind_v3_colors.slate100 |> Css_gen.Color.to_string_css)
                ~keyboard_key_hover_color:
                  (Tailwind_v3_colors.slate900 |> Css_gen.Color.to_string_css)
                ~keyboard_key_width:
                  (Css_gen.Length.to_string_css (`Em_float (4. *. Key.Id.key_width id)))
                ~keyboard_key_height:(Css_gen.Length.to_string_css (`Em_float 4.))
                ()
            ]
          [ key id k ]
      | Some k ->
        let keyboard_key_background_color = Tailwind_v3_colors.slate900 in
        let keyboard_key_background_color_overlay =
          let a, b =
            match k.kc with
            | `Alpha a -> Char.lowercase a, Char.uppercase a
            | `Sym (a, b) -> a, b
            | _ -> failwith "keycode unsupported"
          in
          let freq =
            (Map.find corpus_freq_a a |> Option.value ~default:0.)
            +. (Map.find corpus_freq_a b |> Option.value ~default:0.)
          in
          let (`RGB (r, g, b)) = convert_hex_to_rgb Tailwind_v3_colors.indigo500 in
          let a = Percent.of_mult (freq /. max_value) in
          `RGBA (Css_gen.Color.RGBA.create ~r ~g ~b ~a ())
        in
        let keyboard_key_color = Tailwind_v3_colors.slate100 in
        Vdom.Node.div
          ~attrs:
            [ Style.keyboard_key
            ; Style.Variables.set
                ~keyboard_key_background_color:
                  (keyboard_key_background_color |> Css_gen.Color.to_string_css)
                ~keyboard_key_color:(keyboard_key_color |> Css_gen.Color.to_string_css)
                ~keyboard_key_hover_background_color:
                  (Tailwind_v3_colors.slate100 |> Css_gen.Color.to_string_css)
                ~keyboard_key_hover_color:
                  (Tailwind_v3_colors.slate900 |> Css_gen.Color.to_string_css)
                ~keyboard_key_width:
                  (Css_gen.Length.to_string_css (`Em_float (4. *. Key.Id.key_width id)))
                ~keyboard_key_height:(Css_gen.Length.to_string_css (`Em_float 4.))
                ()
            ]
          [ Vdom.Node.div
              ~attrs:
                [ Style.keyboard_key_overlay
                ; Vdom.Attr.style
                    (Css_gen.background_color keyboard_key_background_color_overlay)
                ]
              [ key id (Some k) ]
          ]
    in
    let tr row =
      row
      |> List.map ~f:td
      |> Computation.all
      |> Computation.map ~f:(Vdom.Node.div ~attrs:[ Style.keyboard_row ])
    in
    let table arrangement =
      arrangement
      |> List.map ~f:tr
      |> Computation.all
      |> Computation.map ~f:(fun keyboard_rows ->
        Vdom.Node.div
          ~attrs:
            [ Style.keyboard
            ; Style.Variables.set
                ()
            ]
          keyboard_rows)
    in
    table arrangement
  in
  let%arr keyboard_vdom = keyboard_vdom in
  Vdom.Node.div ~attrs:[ Style.keyboard_section ] [ keyboard_vdom ]
;;

(* module Action = struct *)
(*   type t = *)
(*     | Random_swap *)
(*     | Swap of (Key.Id.t * Key.Id.t) *)
(*   [@@deriving sexp] *)
(* end *)
(**)
(* module Model = struct *)
(*   type t = *)
(*     { by_id : Key.t Key.Id.Map.t *)
(*     ; by_finger : Key.Set.t Hand_finger.Map.t *)
(*     ; keypairs : Keypair.Set.t *)
(*     ; diff_hand : Keypair.Set.t *)
(*     ; same_hand : Keypair.Set.t *)
(*     ; same_hand_same_finger : Keypair.Set.t *)
(*     ; same_hand_same_finger_same_row : Keypair.Set.t *)
(*     ; same_hand_same_finger_diff_row : Keypair.Set.t Hand_finger.Map.t *)
(*     ; same_hand_diff_finger : Keypair.Set.t *)
(*     ; same_hand_diff_finger_inward : Keypair.Set.t *)
(*     ; same_hand_diff_finger_outward : Keypair.Set.t *)
(*     ; same_hand_diff_finger_same_row : Keypair.Set.t *)
(*     ; same_hand_diff_finger_diff_row : Keypair.Set.t *)
(*     } *)
(*   [@@deriving sexp, equal, compare] *)
(**)
(*   let make x y : (t * (Action.t -> unit Ui_effect.t)) Computation.t = *)
(*     let by_id, gets_sets = *)
(*       let alist = *)
(*         Key.Id.all *)
(*         |> List.map ~f:(fun id -> *)
(*           let col = Key.Id.col id in *)
(*           let row = Key.Id.row id in *)
(*           let hand = Key.Id.hand id in *)
(*           let finger = Key.Id.finger id in *)
(*           let kc = Key.Id.default_kc id in *)
(*           let x = x id ~row ~col in *)
(*           let y = y id ~row ~col in *)
(*           let key = Bonsai.Var.create { Key.id; col; row; hand; finger; kc; x; y } in *)
(*           let set_kc kc = Bonsai.Var.update key ~f:(fun key -> { key with kc }) in *)
(*           let get_kc () = (Bonsai.Var.get key).kc in *)
(*           id, (Bonsai.Var.value key, (get_kc, set_kc))) *)
(*       in *)
(*       let of_id = List.Assoc.map alist ~f:fst |> Key.Id.Map.of_alist_exn in *)
(*       let gets_sets = List.Assoc.map alist ~f:snd |> Key.Id.Map.of_alist_exn in *)
(*       of_id, gets_sets *)
(*     in *)
(*     let by_finger = *)
(*       by_id *)
(*       |> Map.to_alist *)
(*       |> List.fold ~init:Hand_finger.Map.empty ~f:(fun map (id, value) -> *)
(*         let hand = Key.Id.hand id in *)
(*         let finger = Key.Id.finger id in *)
(*         let computation = Bonsai.read value in *)
(*         Map.update map (hand, finger) ~f:(function *)
(*           | None -> [ computation ] *)
(*           | Some acc -> computation :: acc)) *)
(*       |> Map.map ~f:(fun l -> Computation.all l |> Computation.map ~f:Key.Set.of_list) *)
(*       |> Computation.all_map *)
(*     in *)
(*     let swap (a, b) = *)
(*       let a_get, a_set = Map.find_exn gets_sets a in *)
(*       let b_get, b_set = Map.find_exn gets_sets b in *)
(*       let tmp = a_get () in *)
(*       a_set (b_get ()); *)
(*       b_set tmp *)
(*     in *)
(*     let swap = *)
(*       let cache = ref 0 in *)
(*       Ui_effect.of_sync_fun (fun (action : Action.t) -> *)
(*         printf "%d\n" !cache; *)
(*         incr cache; *)
(*         let a, b = *)
(*           match action with *)
(*           | Random_swap -> Key.Id.rand2 () *)
(*           | Swap ab -> ab *)
(*         in *)
(*         swap (a, b)) *)
(*     in *)
(*     let strokes = List.cartesian_product Key.Id.all Key.Id.all in *)
(*     let p_same_hand (a, b) = Hand.equal (Key.Id.hand a) (Key.Id.hand b) in *)
(*     let p_same_finger (a, b) = Finger.equal (Key.Id.finger a) (Key.Id.finger b) in *)
(*     let p_same_row (a, b) = Int.equal (Key.Id.row a) (Key.Id.row b) in *)
(*     let p_dir dir (a, b) = *)
(*       [%equal: [ `In | `Out ]] *)
(*         dir *)
(*         (Finger.roll_direction (Key.Id.finger a) (Key.Id.finger b)) *)
(*     in *)
(*     let f what p = List.filter what ~f:p in *)
(*     let keypairs = f strokes (Fn.const true) in *)
(*     let diff_hand = f strokes (Fn.non p_same_hand) in *)
(*     let same_hand = f strokes p_same_hand in *)
(*     let same_hand_same_finger = f same_hand p_same_finger in *)
(*     let same_hand_same_finger_same_row = f same_hand_same_finger p_same_row in *)
(*     let same_hand_same_finger_diff_row = f same_hand_same_finger (Fn.non p_same_row) in *)
(*     let same_hand_diff_finger = f same_hand (Fn.non p_same_finger) in *)
(*     let same_hand_diff_finger_inward = f same_hand_diff_finger (p_dir `In) in *)
(*     let same_hand_diff_finger_outward = f same_hand_diff_finger (p_dir `Out) in *)
(*     let same_hand_diff_finger_same_row = f same_hand_diff_finger p_same_row in *)
(*     let same_hand_diff_finger_diff_row = f same_hand_diff_finger (Fn.non p_same_row) in *)
(*     let keypair_set what = *)
(*       what *)
(*       |> List.map ~f:(fun (k1, k2) -> *)
(*         let%arr v1 = Map.find_exn by_id k1 *)
(*         and v2 = Map.find_exn by_id k2 in *)
(*         v1, v2) *)
(*       |> Computation.all *)
(*       |> Computation.map ~f:Keypair.Set.of_list *)
(*     in *)
(*     let keypair_map what to_key map_of_alist_multi = *)
(*       what *)
(*       |> List.map ~f:(fun (k1, k2) -> *)
(*         let key = to_key k1 k2 in *)
(*         let data = *)
(*           let%arr v1 = Map.find_exn by_id k1 *)
(*           and v2 = Map.find_exn by_id k2 in *)
(*           v1, v2 *)
(*         in *)
(*         key, data) *)
(*       |> map_of_alist_multi *)
(*       |> Map.map ~f:(fun l -> *)
(*         l |> Computation.all |> Computation.map ~f:Keypair.Set.of_list) *)
(*       |> Computation.all_map *)
(*     in *)
(*     let%sub by_id = Map.map by_id ~f:Bonsai.read |> Computation.all_map in *)
(*     let%sub by_finger = by_finger in *)
(*     let%sub keypairs = keypair_set keypairs in *)
(*     let%sub diff_hand = keypair_set diff_hand in *)
(*     let%sub same_hand = keypair_set same_hand in *)
(*     let%sub same_hand_same_finger = keypair_set same_hand_same_finger in *)
(*     let%sub same_hand_same_finger_same_row = keypair_set same_hand_same_finger_same_row in *)
(*     let%sub same_hand_same_finger_diff_row = *)
(*       keypair_map *)
(*         same_hand_same_finger_diff_row *)
(*         (fun (a : Key.Id.t) _ -> Key.Id.hand a, Key.Id.finger a) *)
(*         Hand_finger.Map.of_alist_multi *)
(*     in *)
(*     let%sub same_hand_diff_finger = keypair_set same_hand_diff_finger in *)
(*     let%sub same_hand_diff_finger_inward = keypair_set same_hand_diff_finger_inward in *)
(*     let%sub same_hand_diff_finger_outward = keypair_set same_hand_diff_finger_outward in *)
(*     let%sub same_hand_diff_finger_same_row = keypair_set same_hand_diff_finger_same_row in *)
(*     let%sub same_hand_diff_finger_diff_row = keypair_set same_hand_diff_finger_diff_row in *)
(*     let%arr by_id = by_id *)
(*     and by_finger = by_finger *)
(*     and keypairs = keypairs *)
(*     and diff_hand = diff_hand *)
(*     and same_hand = same_hand *)
(*     and same_hand_same_finger = same_hand_same_finger *)
(*     and same_hand_same_finger_same_row = same_hand_same_finger_same_row *)
(*     and same_hand_same_finger_diff_row = same_hand_same_finger_diff_row *)
(*     and same_hand_diff_finger = same_hand_diff_finger *)
(*     and same_hand_diff_finger_inward = same_hand_diff_finger_inward *)
(*     and same_hand_diff_finger_outward = same_hand_diff_finger_outward *)
(*     and same_hand_diff_finger_same_row = same_hand_diff_finger_same_row *)
(*     and same_hand_diff_finger_diff_row = same_hand_diff_finger_diff_row in *)
(*     let t = *)
(*       { by_id *)
(*       ; by_finger *)
(*       ; keypairs *)
(*       ; diff_hand *)
(*       ; same_hand *)
(*       ; same_hand_same_finger *)
(*       ; same_hand_same_finger_same_row *)
(*       ; same_hand_same_finger_diff_row *)
(*       ; same_hand_diff_finger *)
(*       ; same_hand_diff_finger_inward *)
(*       ; same_hand_diff_finger_outward *)
(*       ; same_hand_diff_finger_same_row *)
(*       ; same_hand_diff_finger_diff_row *)
(*       } *)
(*     in *)
(*     t, swap *)
(*   ;; *)
(* end *)
(**)
(* let render_keyboard (keyboard : Model.t Value.t) (arrangement : Arrangement.t) = *)
(*   let%sub by_id = *)
(*     let key (id : Key.Id.t) (keyboard : Model.t) = *)
(*       match Map.find keyboard.by_id id with *)
(*       | None -> Vdom.Node.sexp_for_debugging (Key.Id.sexp_of_t id) *)
(*       | Some key -> Vdom.Node.textf "%c" key.kc *)
(*     in *)
(*     let td id = *)
(*       let%arr keyboard = keyboard in *)
(*       Vdom.Node.td [ key id keyboard ] *)
(*     in *)
(*     let tr row = row |> List.map ~f:td |> Computation.all_map' ~f:Vdom.Node.tr in *)
(*     let table arrangement = *)
(*       arrangement |> List.map ~f:tr |> Computation.all_map' ~f:(Vdom.Node.table ~attrs:[]) *)
(*     in *)
(*     table arrangement *)
(*   in *)
(*   let%sub by_finger = *)
(*     let th_td = *)
(*       List.map Hand_finger.all ~f:(fun hand_finger -> *)
(*         let key = Vdom.Node.th [ Vdom.Node.text (Hand_finger.to_string hand_finger) ] in *)
(*         let%arr { Model.by_finger; _ } = keyboard in *)
(*         let set = Map.find_exn by_finger hand_finger in *)
(*         let data = *)
(*           Vdom.Node.td *)
(*             [ Set.map (module Char) set ~f:(fun key -> key.kc) *)
(*               |> Char.Set.sexp_of_t *)
(*               |> Vdom.Node.sexp_for_debugging *)
(*             ] *)
(*         in *)
(*         key, data) *)
(*     in *)
(*     let%sub th = List.map th_td ~f:(Computation.map ~f:fst) |> Computation.all in *)
(*     let%sub td = List.map th_td ~f:(Computation.map ~f:snd) |> Computation.all in *)
(*     let%arr th = th *)
(*     and td = td in *)
(*     Vdom.Node.table *)
(*       [ Vdom.Node.thead [ Vdom.Node.tr th ]; Vdom.Node.tbody [ Vdom.Node.tr td ] ] *)
(*   in *)
(*   let%arr by_id = by_id *)
(*   and by_finger = by_finger in *)
(*   by_id, by_finger *)
(* ;; *)
(**)
(* let component kind = *)
(*   match%sub kind with *)
(*   | `Ansi -> *)
(*     let x id ~row:_ ~col = *)
(*       let col = Float.of_int col in *)
(*       match id with *)
(*       | `Q | `W | `E | `R | `T | `Y | `U | `I | `O | `P -> col *)
(*       | `A | `S | `D | `F | `G | `H | `J | `K | `L | `SEMICOLON -> col +. 0.25 *)
(*       | `Z | `X | `C | `V | `B | `N | `M | `COMMA | `PERIOD | `SLASH -> col +. 0.75 *)
(*     in *)
(*     let y _id ~row ~col:_ = Float.of_int row in *)
(*     let%sub model, inject = Model.make x y in *)
(*     let%sub vdom = render_keyboard model Arrangement.ansi in *)
(*     let%arr model = model *)
(*     and vdom = vdom *)
(*     and inject = inject in *)
(*     model, vdom, inject *)
(* ;; *)
