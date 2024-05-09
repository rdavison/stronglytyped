open! Import

module type S = sig
  module Incr : Incremental.S
  module Layout : Layout.S

  type t =
    { usage : float Incr.t Hand_finger.Map.t (* finger usage*)
    ; sfb : float Incr.t Hand_finger.Map.t (* same finger bigram *)
    ; shb : float Incr.t Hand.Map.t (* same hand bigram *)
    ; sfs : float Incr.t Hand_finger.Map.t (* same finger skipgram *)
    ; shs : float Incr.t Hand.Map.t (* same hand skipgram *)
    ; speed : float Incr.t Hand_finger.Map.t (* finger speed *)
    ; fsb : float Incr.t Hand_finger2.Map.t (* full scissor bigram *)
    ; hsb : float Incr.t Hand_finger2.Map.t (* half scissor bigram *)
    ; fss : float Incr.t Hand_finger2.Map.t (* full scissor skipgram *)
    ; hss : float Incr.t Hand_finger2.Map.t (* half scissor skipgram *)
    ; lsb : float Incr.t Hand_finger2.Map.t (* lateral stretch bigram *)
    ; lss : float Incr.t Hand_finger2.Map.t (* lateral scretch skipgram *)
    ; srb : float Incr.t Hand_finger2.Map.t (* same row adjacent finger bigram *)
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
