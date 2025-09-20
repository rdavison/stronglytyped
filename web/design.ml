open! Core
open! Bonsai_web

let theme = `Light
let light = Tailwind_v3_colors.slate100
let dark = Tailwind_v3_colors.slate900

module Card = struct
  let background_color theme =
    match theme with
    | `Dark -> dark
    | `Light -> light
  ;;

  let color theme =
    match theme with
    | `Dark -> light
    | `Light -> dark
  ;;

  let border_color _theme = Tailwind_v3_colors.slate900

  let attr : Vdom.Attr.t =
    let background_color = background_color theme in
    let color = color theme in
    let border_color = border_color theme in
    [%css
      {|
        padding: 1rem;
        border: 1px solid %{border_color#Css_gen.Color};
        background-color: %{background_color#Css_gen.Color};
        color: %{color#Css_gen.Color};
        box-shadow: 0 1px 2px rgba(0,0,0,.4);
        width: 100%;
      |}]
  ;;
end
