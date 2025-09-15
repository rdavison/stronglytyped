open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
include Analysis.Key

let render_legend kc =
  match kc with
  | `Alpha _ -> Vdom.Node.text (Analysis.Keycode.to_string_upper kc)
  | `Sym (_, _) ->
    let css =
      [%css
        {|
          display: flex;
          flex-direction: column;
        |}]
    in
    Vdom.Node.div
      ~attrs:[ css ]
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
    let css =
      let background_color = Tailwind_v3_colors.slate900 in
      let color = Tailwind_v3_colors.slate100 in
      let width = `Em_float (4. *. Id.key_width id) in
      let height = `Em_float 4. in
      [%css
        {|
          display: flex;
          justify-content: center;
          align-items: center;
          width: %{width#Css_gen.Length};
          height: %{height#Css_gen.Length};
          background-color: %{background_color#Css_gen.Color};
          color: %{color#Css_gen.Color};
          margin: 0.1em;
          border-radius: 0.5rem;
          box-shadow: 0 25px 50px -12px rgb(0 0 0 / 0.25);
          align-items: center;
          text-align: center;
        |}]
    in
    Vdom.Node.div ~attrs:[ css ] [ vdom_optional id (Option.map k ~f:fst) ]
  | Some (k, dnd_attr) ->
    let keyboard_key_background_color = Tailwind_v3_colors.slate900 in
    let keyboard_key_color = Tailwind_v3_colors.slate100 in
    let key_css =
      let background_color = keyboard_key_background_color in
      let color = keyboard_key_color in
      let hover_background_color = Tailwind_v3_colors.slate100 in
      let hover_color = Tailwind_v3_colors.slate900 in
      let width = `Em_float (4. *. Id.key_width id) in
      let height = `Em_float 4. in
      [%css
        {|
          display: flex;
          justify-content: center;
          align-items: center;
          width: %{width#Css_gen.Length};
          height: %{height#Css_gen.Length};
          background-color: %{background_color#Css_gen.Color};
          color: %{color#Css_gen.Color};
          margin: 0.1em;
          transition-property: color, background-color;
          transition-timing-function:  cubic-bezier(0.4, 0, 0.2, 1);
          transition-duration: 300ms;
          transition-timing-function:  cubic-bezier(0, 0, 0.2, 1);
          border-radius: 0.5rem;
          box-shadow: 0 25px 50px -12px rgb(0 0 0 / 0.25);
          align-items: center;
          text-align: center;
          &:hover {
            background-color: %{hover_background_color#Css_gen.Color};
            color: %{hover_color#Css_gen.Color};
          }
        |}]
    in
    let overlay_css =
      let background_color =
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
        let (`RGB (r, g, b)) =
          Util.Color.convert_hex_to_rgb Tailwind_v3_colors.indigo500
        in
        let a = Percent.of_mult (freq /. max_value) in
        `RGBA (Css_gen.Color.RGBA.create ~r ~g ~b ~a ())
      in
      [%css
        {|
          all: inherit;
          margin: unset;
          background-color: %{background_color#Css_gen.Color};
        |}]
    in
    Vdom.Node.div
      ~attrs:[ key_css; dnd_attr ]
      [ Vdom.Node.div ~attrs:[ overlay_css ] [ vdom_optional id (Some k) ] ]
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
