open! Import

type key =
  { var : char Incr.Var.t
  ; finger : int
  ; symmetric_finger : int
  ; hand : int
  ; x : float
  ; y : float
  ; col : int
  ; row : int
  ; symmetric_col : int
  ; layer : int
  ; layer_trigger : int option
  ; modifier : bool
  ; swappable : bool
  ; locked_to : int array
  }

val init
  :  int
  -> code:(int -> char)
  -> finger:(int -> int)
  -> symmetric_finger:(int -> int)
  -> hand:(int -> int)
  -> pos:(int -> float * float)
  -> col:(int -> int)
  -> row:(int -> int)
  -> symmetric_col:(int -> int)
  -> layer:(int -> int)
  -> layer_trigger:(int -> int option)
  -> modifier:(int -> bool)
  -> swappable:(int -> bool)
  -> locked_to:(int -> int array)
  -> key array

type t = key [@@deriving sexp_of]

val ortho42 : t array
val swap : ?on_swap:(int * int -> unit) -> t array -> int -> int -> unit
val rebase : t array -> string -> unit
val scramble : ?on_swap:(int * int -> unit) -> t array -> int -> unit
val length : t array -> int
val layout : t array -> string Incr.t
val layout_pretty : t array -> string Incr.t
val reverse_lookup_table : t array -> int Char.Map.t Incr.t
