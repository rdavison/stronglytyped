open! Import

type t =
  { score : float
  ; layout : string
  ; layout_pretty : string
  ; stats : Stats.t option
  }
[@@deriving sexp_of]

let empty =
  { score = 0.
  ; layout = "abcdefghijklmnopqrtsuvwxyz,.';"
  ; layout_pretty = {|a b c d e f g h i j
k l m n o p q r s t
u v w x y z , . ' ;|}
  ; stats = None
  }
;;

let make_incr ~score ~layout ~layout_pretty =
  let%map_open.Incr score = score
  and layout = layout
  and layout_pretty = layout_pretty in
  { score; layout; layout_pretty; stats = None }
;;
