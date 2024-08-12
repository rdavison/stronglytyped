open! Import

module type S = sig
  module Incr : Incremental.S
  module Layout : Layout.S
  module Stats : Stats.S
  module Score : Score.S

  type t =
    { final_score : Score.t
    ; save_state : Layout.save_state
    }

  val bruteforce
    :  Layout.t
    -> score_obs:Score.t Incr.Observer.t
    -> score_compare:(Score.t -> Score.t -> int)
    -> score_scalarize:(Score.t -> float)
    -> mode:[ `Fast | `Slow ]
    -> t

  val anneal
    :  Layout.t
    -> score_obs:Score.t Incr.Observer.t
    -> score_compare:(Score.t -> Score.t -> int)
    -> score_scalarize:(Score.t -> float)
    -> initial_temperature:float
    -> cooling_rate:float
    -> num_iterations:int
    -> t
end

module type Intf = sig
  module type S = S

  module Make
      (Incr : Incremental.S)
      (Layout : Layout.S with module Incr = Incr)
      (Stats : Stats.S with module Incr = Incr and module Layout = Layout)
      (Score : Score.S
                 with module Incr = Incr
                  and module Layout = Layout
                  and module Stats = Stats) :
    S
      with module Incr = Incr
       and module Layout = Layout
       and module Stats = Stats
       and module Score = Score
end
