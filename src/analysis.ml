open! Import
open! Incr

type t =
  { stats : Stats.t
  ; totals : Totals.t
  ; score : Score.t
  ; layout : string
  ; pretty : Pretty.t
  }

let incr =
  map4 Stats.incr Totals.incr Score.incr Pretty.incr ~f:(fun stats totals score pretty ->
      let layout =
        Root.all |> Array.map ~f:Var.latest_value |> Array.to_list |> String.of_char_list
      in
      { stats; totals; score; layout; pretty })
;;
