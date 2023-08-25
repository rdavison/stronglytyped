open! Import

type t =
  { usage : float Incr.t Hand_finger.Map.t
  ; sfbs : float Incr.t Hand_finger.Map.t
  ; sfss : float Incr.t Hand_finger.Map.t
  ; speed : float Incr.t Hand_finger.Map.t
  ; inrowlls : float Incr.t Hand.Map.t
  ; outrowlls : float Incr.t Hand.Map.t
  ; scissors : float Incr.t
  ; lsb : float Incr.t
  ; slaps : float Incr.t
  }
[@@deriving sexp_of]

val make : Layout.t -> Corpus.t -> t
