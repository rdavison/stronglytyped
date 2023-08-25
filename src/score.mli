open! Import

type info =
  { unweighted : float
  ; weighted : float
  ; final : float
  }
[@@deriving sexp_of]

type t =
  { usage : info
  ; sfb : info
  ; sfs : info
  ; speed : info
  ; inrowlls : info
  ; outrowlls : info
  ; scissors : float
  ; lsb : float
  ; slaps : float
  }
[@@deriving sexp_of]

type config =
  { usage : float Hand_finger.Map.t -> Hand_finger.t -> float -> float
  ; aggregate_usage :
      float Hand_finger.Map.t -> unweighted:float -> weighted:float -> float
  ; sfb : float Hand_finger.Map.t -> Hand_finger.t -> float -> float
  ; aggregate_sfb : float Hand_finger.Map.t -> unweighted:float -> weighted:float -> float
  ; sfs : float Hand_finger.Map.t -> Hand_finger.t -> float -> float
  ; aggregate_sfs : float Hand_finger.Map.t -> unweighted:float -> weighted:float -> float
  ; speed : float Hand_finger.Map.t -> Hand_finger.t -> float -> float
  ; aggregate_speed :
      float Hand_finger.Map.t -> unweighted:float -> weighted:float -> float
  ; inrowlls : float Hand.Map.t -> Hand.t -> float -> float
  ; aggregate_inrowlls : float Hand.Map.t -> unweighted:float -> weighted:float -> float
  ; outrowlls : float Hand.Map.t -> Hand.t -> float -> float
  ; aggregate_outrowlls : float Hand.Map.t -> unweighted:float -> weighted:float -> float
  ; scissors : float -> float
  ; lsb : float -> float
  ; slaps : float -> float
  }

val make : Stats.t -> config:config -> t Incr.t
val final_sum : t Incr.t -> float Incr.t
val default_config : config
