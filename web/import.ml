open! Core
open! Bonsai_web

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
