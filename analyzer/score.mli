open! Import

module Ast : sig
  type t =
    | Var of string
    | Prj of t * string
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Div of t * t
    | Pow of t * float
  [@@deriving sexp]

  val compile : t -> (float Incr.t, string) result
end

val sfb : float Hf.Map.t Incr.t
val sfb_total : float Incr.t
val dsfb : float Hf.Map.t Incr.t
val dsfb_total : float Incr.t
val speed : float Hf.Map.t Incr.t
val speed_total : float Incr.t
val lsb : float Hand.Map.t Incr.t
val lsb_total : float Incr.t
val dshrc : float Hand.Map.t Incr.t
val dshrc_total : float Incr.t
val incr : float Incr.t