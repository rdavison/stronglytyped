open! Import

module type S = sig
  module Incr : Incremental.S
  module Layout : Layout.S
  module Stats : Stats.S

  type info =
    { unweighted : float
    ; weighted : float
    }
  [@@deriving sexp_of]

  type t =
    { usage : info option
    ; sfb : info option
    ; sfs : info option
    ; speed : info option
    ; inrowlls : info option
    ; outrowlls : info option
    ; scissors : info option
    ; lsb : info option
    ; slaps : info option
    ; badredirs : info option
    ; badtrills : info option
    ; layer_transitions : info option
    ; layer_trigger_s129 : info option
    }
  [@@deriving sexp_of]

  val make
    :  ?usage:(float Hand_finger.Map.t -> info)
    -> ?sfb:(float Hand_finger.Map.t -> info)
    -> ?sfs:(float Hand_finger.Map.t -> info)
    -> ?speed:(float Hand_finger.Map.t -> info)
    -> ?inrowlls:(float Hand.Map.t -> info)
    -> ?outrowlls:(float Hand.Map.t -> info)
    -> ?scissors:(float -> info)
    -> ?lsb:(float -> info)
    -> ?slaps:(float -> info)
    -> ?badredirs:(float -> info)
    -> ?badtrills:(float -> info)
    -> ?layer_transitions:(float -> info)
    -> ?layer_trigger_s129:(float -> info)
    -> Stats.t
    -> t Incr.t

  val final_sum : t Incr.t -> float Incr.t
  val default_config : Stats.t -> t Incr.t
end

module Make
    (Incr : Incremental.S)
    (Layout : Layout.S with module Incr = Incr)
    (Stats : Stats.S with module Incr = Incr and module Layout = Layout) :
  S with module Incr = Incr and module Layout = Layout and module Stats = Stats
