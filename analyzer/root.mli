open! Import

type 'var key =
  { var : 'var
  ; finger : Finger.t
  ; hand : Hand.t
  ; x : float
  ; y : float
  ; col : int
  ; row : int
  ; layer : int
  ; layer_trigger : int option
  ; modifier : bool
  ; swappable : bool
  ; locked_to : int array
  }

val init
  :  int
  -> code:(int -> Code.t)
  -> finger:(int -> Finger.t)
  -> hand:(int -> Hand.t)
  -> pos:(int -> float * float)
  -> col:(int -> int)
  -> row:(int -> int)
  -> layer:(int -> int)
  -> layer_trigger:(int -> int option)
  -> modifier:(int -> bool)
  -> swappable:(int -> bool)
  -> locked_to:(int -> int array)
  -> Code.t Incr.Var.t key array

type t = Code.t Incr.Var.t key [@@deriving sexp_of]

val ortho42 : t array
val swap : ?on_swap:(int * int -> unit) -> t array -> int -> int -> unit
val rebase : t array -> string -> unit
val scramble : ?on_swap:(int * int -> unit) -> t array -> int -> unit
val length : t array -> int
val layout : t array -> string Incr.t
val layout_pretty : t array -> string Incr.t
val reverse_lookup_table : t array -> int Char.Map.t Incr.t
