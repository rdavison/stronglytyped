open! Core
open! Bonsai_web

module Mode : sig
  type t =
    | Auto
    | Manual
  [@@deriving sexp, equal, compare]

  val start : t Bonsai.t -> f:unit Ui_effect.t Bonsai.t -> Bonsai.graph -> unit

  val component
    :  theme:[ `Dark | `Light ] Bonsai.t
    -> Bonsai.graph
    -> t Bonsai.t * Vdom.Node.t Bonsai.t
end
