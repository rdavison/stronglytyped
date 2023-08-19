open! Import

type t =
  { sfbs : float Incr.t Hand_finger.Map.t
  ; sfss : float Incr.t Hand_finger.Map.t
  }
[@@deriving sexp_of]

val make : Layout.t -> Corpus.t -> t
