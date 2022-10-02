open! Import

type t =
  { score : float
  ; layout : string
  ; pretty : Pretty.t
  }

let incr =
  Incr.map2 Score.incr Pretty.incr ~f:(fun score pretty ->
      let layout =
        Root.all
        |> Array.map ~f:Incr.Var.latest_value
        |> Array.to_list
        |> String.of_char_list
      in
      { score; layout; pretty })
;;
