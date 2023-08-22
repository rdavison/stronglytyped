open! Import

type t = float Incr.t

type config =
  { usage : Hand_finger.t -> float -> float
  ; aggregate_usage : unweighted:float -> weighted:float -> float
  ; sfb : Hand_finger.t -> float -> float
  ; aggregate_sfb : unweighted:float -> weighted:float -> float
  ; sfs : Hand_finger.t -> float -> float
  ; aggregate_sfs : unweighted:float -> weighted:float -> float
  ; speed : Hand_finger.t -> float -> float
  ; aggregate_speed : unweighted:float -> weighted:float -> float
  ; inrowlls : Hand.t -> float -> float
  ; aggregate_inrowlls : unweighted:float -> weighted:float -> float
  ; outrowlls : Hand.t -> float -> float
  ; aggregate_outrowlls : unweighted:float -> weighted:float -> float
  }

val make : Stats.t -> config:config -> t
val default_config : config
