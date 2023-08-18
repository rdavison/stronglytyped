open! Import

type t = (Key.t * Code.t Incr.Var.t) array [@@deriving sexp_of]

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
  -> locked_to:(int -> int list)
  -> t

val ortho42 : t
val swap : ?on_swap:(int * int -> unit) -> t -> int -> int -> unit
val rebase : t -> string -> unit
val scramble : ?on_swap:(int * int -> unit) -> t -> int -> unit
val length : t -> int
val all : (string * string) list
val set : t -> [ `Name of string | `Layout of string ] -> unit
