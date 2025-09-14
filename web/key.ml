open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
include Analysis.Key

module Style =
  [%css
    stylesheet
      {|
        .keyboard-key {
          display: flex;
          justify-content: center;
          align-items: center;
          width: var(--keyboard-key-width);
          height: var(--keyboard-key-height);
          background-color: var(--keyboard-key-background-color);
          color: var(--keyboard-key-color);
          margin: 0.1em;
          transition-property: color, background-color;
          transition-timing-function:  cubic-bezier(0.4, 0, 0.2, 1);
          transition-duration: 300ms;
          transition-timing-function:  cubic-bezier(0, 0, 0.2, 1);
          border-radius: 0.5rem;
          box-shadow: 0 25px 50px -12px rgb(0 0 0 / 0.25);
          align-items: center;
          text-align: center;
        }

        .keyboard-key-overlay {
          all: inherit;
          margin: unset;
        }

        .keyboard-key:hover {
          background-color: var(--keyboard-key-hover-background-color);
          color: var(--keyboard-key-hover-color);
        }

        .keyboard-key-label {
          display: flex;
          flex-direction: column;
        }
|}]

let render_legend kc =
  match kc with
  | `Alpha _ -> Vdom.Node.text (Analysis.Keycode.to_string_upper kc)
  | `Sym (_, _) ->
    Vdom.Node.div
      ~attrs:[ Style.keyboard_key_label ]
      [ Vdom.Node.div [ Vdom.Node.text (Analysis.Keycode.to_string_upper kc) ]
      ; Vdom.Node.div [ Vdom.Node.text (Analysis.Keycode.to_string_lower kc) ]
      ]
  | `Legend legend -> Vdom.Node.text legend
  | `Power -> Vdom.Node.text "O"
;;

let vdom_optional (id : Id.t) (key : t option) =
  match key with
  | None -> render_legend (Analysis.Key.Id.default_kc id)
  | Some key -> render_legend key.kc
;;

let vdom id k corpus_freq_a max_value =
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
              (Css_gen.Length.to_string_css (`Em_float (4. *. Id.key_width id)))
            ~keyboard_key_height:(Css_gen.Length.to_string_css (`Em_float 4.))
            ()
        ]
      [ vdom_optional id (Option.map k ~f:fst) ]
  | Some (k, dnd_attr) ->
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
      let (`RGB (r, g, b)) = Util.Color.convert_hex_to_rgb Tailwind_v3_colors.indigo500 in
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
              (Css_gen.Length.to_string_css (`Em_float (4. *. Id.key_width id)))
            ~keyboard_key_height:(Css_gen.Length.to_string_css (`Em_float 4.))
            ()
        ; dnd_attr
        ]
      [ Vdom.Node.div
          ~attrs:
            [ Style.keyboard_key_overlay
            ; Vdom.Attr.style
                (Css_gen.background_color keyboard_key_background_color_overlay)
            ]
          [ vdom_optional id (Some k) ]
      ]
;;

let component id ~keyboard ~corpus_freq_a ~max_value ~dnd _graph =
  let%arr id = id
  and keyboard = keyboard
  and corpus_freq_a = corpus_freq_a
  and max_value = max_value
  and dnd_source = dnd >>| Bonsai_web_ui_drag_and_drop.source
  and dnd_target = dnd >>| Bonsai_web_ui_drag_and_drop.drop_target in
  let dnd_attr id = Vdom.Attr.( @ ) (dnd_source ~id) (dnd_target ~id) in
  let k = Map.find keyboard id |> Option.map ~f:(fun key -> key, dnd_attr id) in
  vdom id k corpus_freq_a max_value
;;
