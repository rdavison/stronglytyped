open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
include Analysis.Key

let render_legend kc =
  match kc with
  | `Alpha _ -> Vdom.Node.text (Analysis.Keycode.to_string_upper kc)
  | `Sym (_, _) ->
    Vdom.Node.div
      ~attrs:
        [ [%css
            {|
              display: flex;
              flex-direction: column;
            |}]
        ]
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

let vdom ?dnd_element id k corpus_freq_a max_value theme =
  let common_css =
    let width = `Em_float (2.5 *. Id.key_width id) in
    let height = `Em_float 2.5 in
    [%css
      {|
          display: flex;
          justify-content: center;
          align-items: center;
          width: %{width#Css_gen.Length};
          height: %{height#Css_gen.Length};
          margin: 0.1em;
          border-radius: 0.5rem;
          align-items: center;
          text-align: center;
          user-select: none;
        |}]
  in
  match k with
  | None ->
    let css =
      let background_color =
        match theme with
        | `Light -> Design.dark
        | `Dark -> Design.light
      in
      let color =
        match theme with
        | `Light -> Design.light
        | `Dark -> Design.dark
      in
      let shadow_color =
        Util.Color.hex2rgba (Design.accent `Light) ~a:(Percent.of_mult 0.25)
      in
      [%css
        {|
          background-color: %{background_color#Css_gen.Color};
          color: %{color#Css_gen.Color};
          box-shadow: 0 25px 50px -12px %{shadow_color#Css_gen.Color};
        |}]
    in
    Vdom.Node.div ~attrs:[ common_css; css ] [ vdom_optional id (Option.map k ~f:fst) ]
  | Some (k, dnd_attr) ->
    let keyboard_key_background_color =
      match theme with
      | `Light -> Design.dark
      | `Dark -> Design.light
    in
    let keyboard_key_color =
      match theme with
      | `Light -> Design.light
      | `Dark -> Design.dark
    in
    let hover_color =
      match theme with
      | `Light -> Design.dark
      | `Dark -> Design.light
    in
    let hover_background_color =
      match theme with
      | `Light -> Design.light
      | `Dark -> Design.dark
    in
    let box_shadow =
      match dnd_element with
      | None -> "0 25px 50px -12px rgb(0 0 0 / 0.25)"
      | Some _ -> "7px 7px 4px -2px rgb(0 0 0 / 0.50)"
    in
    let key_css =
      let background_color = keyboard_key_background_color in
      let color = keyboard_key_color in
      [%css
        {|
          background-color: %{background_color#Css_gen.Color};
          color: %{color#Css_gen.Color};
          transition-property: color, background-color;
          transition-timing-function:  cubic-bezier(0.4, 0, 0.2, 1);
          transition-duration: 300ms;
          transition-timing-function:  cubic-bezier(0, 0, 0.2, 1);
          box-shadow: %{box_shadow};
          &:hover {
            background-color: %{hover_background_color#Css_gen.Color};
            color: %{hover_color#Css_gen.Color};
          }
        |}]
    in
    let overlay_css =
      let active_background_color =
        match theme with
        | `Light -> Tailwind_v3_colors.amber500
        | `Dark -> Tailwind_v3_colors.amber500
      in
      let active_color =
        match theme with
        | `Light -> Design.dark
        | `Dark -> Design.light
      in
      let overlay_color =
        Design.accent theme
        (* match theme with *)
        (* | `Light -> Tailwind_v3_colors.blue500 *)
        (* | `Dark -> Tailwind_v3_colors.blue500 *)
      in
      let background_color =
        match dnd_element with
        | Some _ -> (active_background_color :> Css_gen.Color.t)
        | None ->
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
          let (`RGB (r, g, b)) = Util.Color.hex2rgb overlay_color in
          let a = Percent.of_mult (freq /. max_value) in
          `RGBA (Css_gen.Color.RGBA.create ~r ~g ~b ~a ())
      in
      let color =
        match dnd_element with
        | Some _ -> Css_gen.Color.to_string_css (active_color :> Css_gen.Color.t)
        | None -> "inherit"
      in
      [%css
        {|
          all: inherit;
          margin: unset;
          background-color: %{background_color#Css_gen.Color};
          color: %{color};
          &:hover {
            background-color: %{hover_background_color#Css_gen.Color};
            color: %{hover_color#Css_gen.Color};
          }
          &:active {
            background-color: %{active_background_color#Css_gen.Color};
            color: %{active_color#Css_gen.Color};
          }
        |}]
    in
    Vdom.Node.div
      ~attrs:[ common_css; key_css; dnd_attr ]
      [ Vdom.Node.div ~attrs:[ overlay_css ] [ vdom_optional id (Some k) ] ]
;;

let component ?dnd_element id ~keyboard ~corpus_freq_a ~max_value ~dnd ~theme _graph =
  let%arr id = id
  and keyboard = keyboard
  and corpus_freq_a = corpus_freq_a
  and max_value = max_value
  and dnd_source = dnd >>| Bonsai_web_ui_drag_and_drop.source
  and dnd_target = dnd >>| Bonsai_web_ui_drag_and_drop.drop_target
  and theme = theme in
  let dnd_attr id = Vdom.Attr.( @ ) (dnd_source ~id) (dnd_target ~id) in
  let k = Map.find keyboard id |> Option.map ~f:(fun key -> key, dnd_attr id) in
  vdom ?dnd_element id k corpus_freq_a max_value theme
;;

let dragged_component = component ~dnd_element:()
let component = component ?dnd_element:None
