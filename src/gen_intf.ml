open! Import

module type S = sig
  module Incr : Incremental.S
  module Layout : Layout.S
  module Stats : Stats.S
  module Score : Score.S

  type t =
    { final_score : float
    ; save_state : Layout.save_state
    }

  val bruteforce
    :  Layout.t
    -> final_score_obs:float Incr.Observer.t
    -> mode:[ `Fast | `Slow ]
    -> t

  val anneal : Layout.t -> final_score_obs:float Incr.Observer.t -> t
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
