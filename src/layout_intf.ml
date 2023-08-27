open! Import

module type S = sig
  module Incr : Incremental.S

  type var =
    { swappability : Swappability.t
    ; code : Code.t
    }
  [@@deriving sexp, compare, equal]

  type t =
    { num_keys_per_layer : int
    ; num_layers : int
    ; num_cols : int
    ; num_rows : int
    ; num_keys : int
    ; keys : (Key.t * var Incr.Var.t) array
    }
  [@@deriving sexp_of]

  type save_state = var array [@@deriving sexp, compare, equal]
  type swap = (int * var Incr.Var.t) * (int * var Incr.Var.t) [@@deriving sexp_of]

  val init
    :  int
    -> code:(int -> Code.t)
    -> finger:(int -> Finger.t)
    -> hand:(int -> Hand.t)
    -> pos:(int -> float * float)
    -> col:(int -> int)
    -> row:(int -> int)
    -> layer:(int -> int)
    -> layer_trigger:(int -> hand:Hand.t -> int option)
    -> modifier:(int -> bool)
    -> swappability:(int -> Swappability.t)
    -> t

  val ortho42 : unit -> t
  val ansi : unit -> t
  val swap : swap list -> unit
  val swaps : t -> int -> int -> swap list
  val scramble : t -> int -> unit
  val all : (string * string) list

  (* val valid_swaps : t -> (int * int) array *)
  (* val valid_swaps2 : t -> ((int * int) * (int * int)) array *)
  val pretty_string : t -> string
  val save : t -> save_state
  val load : t -> save_state -> unit
  val layer_offset : t -> int -> int * int
  val index : t -> layer:int -> offset:int -> int
  val tower : t -> int -> int list
end
