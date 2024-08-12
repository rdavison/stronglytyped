open! Import

module type S0 = sig
  module Info : sig
    type t =
      { unweighted : float
      ; weighted : float
      }
    [@@deriving sexp]

    val weighted : t -> float
    val unweighted : t -> float
    val compare_unweighted : t -> t -> int
    val compare_weighted : t -> t -> int
  end

  type t =
    { usage : Info.t option
    ; sfb : Info.t option
    ; shb : Info.t option
    ; sfs : Info.t option
    ; shs : Info.t option
    ; speed : Info.t option
    ; fsb : Info.t option
    ; hsb : Info.t option
    ; fss : Info.t option
    ; hss : Info.t option
    ; lsb : Info.t option
    ; lss : Info.t option
    ; srb : Info.t option
    ; srs : Info.t option
    ; srspeed : Info.t option
    }
  [@@deriving sexp]

  val scalarize : (Info.t -> float) -> t -> float
  val compare_pareto : (Info.t -> float) -> t -> t -> int
  val compare_scalarized : (Info.t -> float) -> t -> t -> int
end

module type S = sig
  module Incr : Incremental.S
  module Layout : Layout.S
  module Stats : Stats.S
  include S0

  val make
    :  ?usage:(float Hand_finger.Map.t -> Info.t)
    -> ?sfb:(float Hand_finger.Map.t -> Info.t)
    -> ?shb:(float Hand.Map.t -> Info.t)
    -> ?sfs:(float Hand_finger.Map.t -> Info.t)
    -> ?shs:(float Hand.Map.t -> Info.t)
    -> ?speed:(float Hand_finger.Map.t -> Info.t)
    -> ?fsb:(float Hand_finger2.Map.t -> Info.t)
    -> ?hsb:(float Hand_finger2.Map.t -> Info.t)
    -> ?fss:(float Hand_finger2.Map.t -> Info.t)
    -> ?hss:(float Hand_finger2.Map.t -> Info.t)
    -> ?lsb:(float Hand_finger2.Map.t -> Info.t)
    -> ?lss:(float Hand_finger2.Map.t -> Info.t)
    -> ?srb:(float Hand_finger2.Map.t -> Info.t)
    -> ?srs:(float Hand_finger2.Map.t -> Info.t)
    -> ?srspeed:(float Hand_finger2.Map.t -> Info.t)
    -> Stats.t
    -> t Incr.t

  val default_config : Stats.t -> t Incr.t
end

module type Intf = sig
  module type S = S

  include S0

  module Make
      (Incr : Incremental.S)
      (Layout : Layout.S with module Incr = Incr)
      (Stats : Stats.S with module Incr = Incr and module Layout = Layout) :
    S
      with module Incr = Incr
       and module Layout = Layout
       and module Stats = Stats
       and module Info = Info
       and type t = t
end
