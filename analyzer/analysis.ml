open! Import

type t =
  { score : float
  ; layout : string
  ; layout_pretty : string
  ; stats : Stats.t option
  }
[@@deriving sexp]

let incr =
  let%map_open.Incr score = Score.incr
  and layout = Root.layout
  and layout_pretty = Root.layout_pretty in
  { score; layout; layout_pretty; stats = None }
;;
