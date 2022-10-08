open! Import

type t =
  { score : float
  ; layout : string
  ; layout_pretty : string
  ; temperature : float
  ; cooling_factor : float
  ; best : (float * string) list
  }
[@@deriving sexp]

let incr =
  let%map_open.Incr score = Score.incr
  and layout = Root.layout
  and layout_pretty = Root.layout_pretty
  and temperature = Config.Incr.temperature
  and cooling_factor = Config.Incr.cooling_factor
  and best = Layout.best in
  { score; layout; layout_pretty; temperature; cooling_factor; best }
;;
