open! Core
open! Bonsai_web

module Mode : sig
  type t =
    | Auto
    | Manual
  [@@deriving sexp, equal, compare]

  val component : Bonsai.graph -> t Bonsai.t * Vdom.Node.t Bonsai.t
end
