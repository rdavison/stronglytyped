open! Core
open! Bonsai

module Mode : sig
  type t =
    [ `Auto
    | `Manual
    ]
  [@@deriving sexp, compare, equal, enumerate]

  val start : t Bonsai.t -> f:unit Ui_effect.t Bonsai.t -> Bonsai.graph -> unit

  val state_machine
    :  default_model:t
    -> Bonsai.graph
    -> t Bonsai.t * (unit -> unit Ui_effect.t) Bonsai.t
end
