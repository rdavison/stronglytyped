open! Import

type t = float Incr.t

type weights =
  { sfb : Hand_finger.t -> float
  ; sfbs : float
  ; sfs : Hand_finger.t -> float
  ; sfss : float
  ; inrowlls : Hand.t -> float
  ; inrowllss : float
  ; outrowlls : Hand.t -> float
  ; outrowllss : float
  }

val make : Stats.t -> weights:weights -> t
val default_weights : weights
