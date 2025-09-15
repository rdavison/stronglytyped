open! Core
open! Bonsai

module Mode : sig
  type t =
    | Auto
    | Manual
  [@@deriving sexp, compare, equal]

  val start : t Bonsai.t -> f:unit Ui_effect.t Bonsai.t -> Bonsai.graph -> unit
end
