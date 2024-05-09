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
    ; shb : info option
    ; sfs : info option
    ; shs : info option
    ; speed : info option
    ; fsb : info option
    ; hsb : info option
    ; fss : info option
    ; hss : info option
    ; lsb : info option
    ; lss : info option
    ; srb : info option
    }
  [@@deriving sexp_of]

  val make
    :  ?usage:(float Hand_finger.Map.t -> info)
    -> ?sfb:(float Hand_finger.Map.t -> info)
    -> ?shb:(float Hand.Map.t -> info)
    -> ?sfs:(float Hand_finger.Map.t -> info)
    -> ?shs:(float Hand.Map.t -> info)
    -> ?speed:(float Hand_finger.Map.t -> info)
    -> ?fsb:(float Hand_finger2.Map.t -> info)
    -> ?hsb:(float Hand_finger2.Map.t -> info)
    -> ?fss:(float Hand_finger2.Map.t -> info)
    -> ?hss:(float Hand_finger2.Map.t -> info)
    -> ?lsb:(float Hand_finger2.Map.t -> info)
    -> ?lss:(float Hand_finger2.Map.t -> info)
    -> ?srb:(float Hand_finger2.Map.t -> info)
    -> Stats.t
    -> t Incr.t

  val final_sum : t Incr.t -> float Incr.t
  val default_config : Stats.t -> t Incr.t
end

module type Intf = sig
  module type S = S

  module Make
      (Incr : Incremental.S)
      (Layout : Layout.S with module Incr = Incr)
      (Stats : Stats.S with module Incr = Incr and module Layout = Layout) :
    S with module Incr = Incr and module Layout = Layout and module Stats = Stats
end
