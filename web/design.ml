open! Core
open! Bonsai_web

let card : Vdom.Attr.t =
  [%css
    {|
      padding: 1rem;
      border: 1px solid %{Tailwind_v3_colors.slate900#Css_gen.Color};
      background-color: %{Tailwind_v3_colors.slate100#Css_gen.Color};
      box-shadow: 0 1px 2px rgba(0,0,0,.4);
      width: 100%;
    |}]
;;
