open! Import

module Ast : sig
  type t =
    | Var of string
    | Num of float
    | Prj of t * string
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Div of t * t
    | Pow of t * t
  [@@deriving sexp]

  type map =
    [ `Hand of float Hand.Map.t Incr.t
    | `Hf of float Hf.Map.t Incr.t
    ]

  val compile : t -> (float Incr.t, string) result
  val of_syntax : string -> (t, string) result
  val uniq_vars : t -> string list
  val all_vars : (string, map * float Incr.t) List.Assoc.t

  val stats_of_vars
    :  string list
    -> ((string * (float Hf.Map.t * float)) list
       * (string * (float Hand.Map.t * float)) list)
       Incr.t
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