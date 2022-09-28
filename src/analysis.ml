open! Import
open! Incr

type t =
  { score : Score.t
  ; layout : string
  ; pretty : Pretty.t
  }

let incr =
  map2 Score.incr Pretty.incr ~f:(fun score pretty ->
      let layout =
        Root.all |> Array.map ~f:Var.latest_value |> Array.to_list |> String.of_char_list
      in
      { score; layout; pretty })
;;
