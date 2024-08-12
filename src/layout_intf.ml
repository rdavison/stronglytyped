open! Import

module S0 = struct
  type var =
    { swappability : Swappability.t
    ; code : Code.t
    }
  [@@deriving sexp, compare, equal]

  type save_state = var array [@@deriving sexp, compare, equal]
end

module type S = sig
  module Incr : Incremental.S
  include module type of S0

  type info =
    { num_keys_per_layer : int
    ; num_layers : int
    ; num_cols : int
    ; num_rows : int
    ; num_keys : int
    }
  [@@deriving sexp]

  type id =
    [ `Ansi
    | `Ortho42
    | `Simple30
    | `Custom of info
    ]
  [@@deriving sexp]

  val info_of_id : id -> info

  type t =
    { id : id
    ; keys : (Key.t * var Incr.Var.t) array
    }
  [@@deriving sexp_of]

  type swap = (int * var Incr.Var.t) * (int * var Incr.Var.t) [@@deriving sexp_of]

  val init
    :  id:id
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

  (* val init_simple : ?finger_map:Fingermap.t -> string -> t *)
  val ortho42 : unit -> t
  val ansi : unit -> t
  val simple30 : stagger:bool -> layout_str:string -> fingermap:Fingermap.t -> t
  val swap : swap list -> unit
  val swaps : t -> int -> int -> swap list
  val scramble : t -> int -> unit
  val all : (string * string) list

  (* val valid_swaps : t -> (int * int) array *)
  (* val valid_swaps2 : t -> ((int * int) * (int * int)) array *)
  val pretty_string : t -> string
  val save : t -> save_state
  val load : t -> save_state -> unit
  val layer_offset : info -> int -> int * int
  val index : info -> layer:int -> offset:int -> int
  val tower : info -> int -> int list
end

module type Intf = sig
  include module type of S0

  module type S = S

  module Make (Incr : Incremental.S) :
    S with module Incr = Incr and type var = var and type save_state = save_state
end
