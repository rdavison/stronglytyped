open! Import

module type S = sig
  module Incr : Incremental.S
  module Layout : Layout.S

  type t =
    { usage : float Incr.t Hand_finger.Map.t
    ; sfbs : float Incr.t Hand_finger.Map.t
    ; sfss : float Incr.t Hand_finger.Map.t
    ; speed : float Incr.t Hand_finger.Map.t
    ; inrowlls : float Incr.t Hand.Map.t
    ; outrowlls : float Incr.t Hand.Map.t
    ; scissors : float Incr.t
    ; lsb : float Incr.t
    ; termi : float Incr.t
    ; slaps : float Incr.t
    ; badredirs : float Incr.t
    ; badtrills : float Incr.t
    ; layer_transitions : float Incr.t
    ; layer_trigger_s129 : float Incr.t
    }
  [@@deriving sexp_of]

  val make : Layout.t -> Corpus.t -> t
  val pretty_string : t -> string Incr.t
end

module type Intf = sig
  module type S = S

  module Make (Incr : Incremental.S) (Layout : Layout.S with module Incr = Incr) :
    S with module Incr = Incr and module Layout = Layout
end
