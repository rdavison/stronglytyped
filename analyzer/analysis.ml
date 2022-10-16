open! Import

type t =
  { score : float
  ; layout : string
  ; layout_pretty : string
  ; stats : Stats.t option
  }
[@@deriving sexp, compare]

let empty =
  { score = 0.
  ; layout = "abcdefghijklmnopqrtsuvwxyz,.';"
  ; layout_pretty = {|a b c d e f g h i j
k l m n o p q r s t
u v w x y z , . ' ;|}
  ; stats = None
  }
;;

let incr =
  let%map_open.Incr score = Score.incr
  and layout = Root.layout
  and layout_pretty = Root.layout_pretty
  and stats = Stats.incr in
  { score; layout; layout_pretty; stats = Some stats }
;;
