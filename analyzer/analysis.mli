open! Import

type t =
  { score : float
  ; layout : string
  ; layout_pretty : string
  ; stats : Stats.t option
  }
[@@deriving sexp_of]

val empty : t

val make_incr
  :  score:float Incr.t
  -> layout:string Incr.t
  -> layout_pretty:string Incr.t
  -> t Incr.t
