open! Import

type t =
  { num_keys_per_layer : int
  ; num_layers : int
  ; num_cols : int
  ; num_rows : int
  ; keys : (Key.t * Code.t Incr.Var.t) array
  }
[@@deriving sexp_of]

type save_state = Code.t array [@@deriving sexp]

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
  -> swappable:(int -> code:Code.t -> modifier:bool -> bool)
  -> locked_to:(int -> int list)
  -> t

val ortho42 : unit -> t
val ansi : unit -> t
val swap : ?on_swap:(int * int -> unit) -> t -> int -> int -> unit
val scramble : ?on_swap:(int * int -> unit) -> t -> int -> unit
val all : (string * string) list
val set : t -> [ `Name of string | `Layout of string ] -> unit
val valid_swaps : t -> (int * int) array
val valid_swaps2 : t -> ((int * int) * (int * int)) array
val pretty_string : t -> string
val save : t -> save_state
val load : t -> save_state -> unit
