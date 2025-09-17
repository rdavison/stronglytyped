open! Core
open! Bonsai_web

let theme = `Light

let card : Vdom.Attr.t =
  let light = Tailwind_v3_colors.slate100 in
  let dark = Tailwind_v3_colors.slate900 in
  let background_color =
    match theme with
    | `Dark -> dark
    | `Light -> light
  in
  let color =
    match theme with
    | `Dark -> light
    | `Light -> dark
  in
  [%css
    {|
      padding: 1rem;
      border: 1px solid %{Tailwind_v3_colors.slate900#Css_gen.Color};
      background-color: %{background_color#Css_gen.Color};
      color: %{color#Css_gen.Color};
      box-shadow: 0 1px 2px rgba(0,0,0,.4);
      width: 100%;
    |}]
;;
