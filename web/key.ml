open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
include Analysis.Key

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
      [ vdom_optional id k ]
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
