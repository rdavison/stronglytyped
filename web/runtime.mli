open! Core
open! Bonsai_web

module Mode : sig
  type t =
    | Auto
    | Manual
  [@@deriving sexp, equal, compare]

  val component : (t * Vdom.Node.t) Computation.t
end
