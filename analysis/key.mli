open! Core

module Id : sig
  type t =
    [ `ESC
    | `F1
    | `F2
    | `F3
    | `F4
    | `F5
    | `F6
    | `F7
    | `F8
    | `F9
    | `F10
    | `F11
    | `F12
    | `POWER
    | `TILDE
    | `_1
    | `_2
    | `_3
    | `_4
    | `_5
    | `_6
    | `_7
    | `_8
    | `_9
    | `_0
    | `HYPHEN
    | `EQUAL
    | `BACKSPACE
    | `TAB
    | `Q
    | `W
    | `E
    | `R
    | `T
    | `Y
    | `U
    | `I
    | `O
    | `P
    | `OPEN_BRACKET
    | `CLOSE_BRACKET
    | `BACKSLASH
    | `CAPSLOCK
    | `A
    | `S
    | `D
    | `F
    | `G
    | `H
    | `J
    | `K
    | `L
    | `SEMICOLON
    | `QUOTE
    | `ENTER
    | `LEFT_SHIFT
    | `Z
    | `X
    | `C
    | `V
    | `B
    | `N
    | `M
    | `COMMA
    | `PERIOD
    | `SLASH
    | `RIGHT_SHIFT
    | `FN
    | `LEFT_CTRL
    | `LEFT_OPTION
    | `LEFT_COMMAND
    | `SPACE
    | `RIGHT_COMMAND
    | `RIGHT_OPTION
    | `LEFT
    | `DOWN
    | `UP
    | `RIGHT
    ]
  [@@deriving sexp, bin_io, compare, equal, enumerate]

  include Comparable.S_binable with type t := t

  val default_kc : t -> Keycode.t
  val key_width : t -> float
  val row : t -> int
  val col : t -> int
  val hand : t -> Hand.t
  val finger : t -> Finger.t
  val all_var : t list
  val rand2 : unit -> t * t

  module Ansi : sig
    val x : t -> row:int -> col:int -> float
    val y : t -> row:int -> col:int -> float
  end

  module Pair : sig
    type nonrec t = (t, t) Tuple2.t

    type nonrec comparator_witness =
      (comparator_witness, comparator_witness) Tuple2.comparator_witness

    include
      Comparable.S with type t := t and type comparator_witness := comparator_witness

    val all : t list
  end
end

type t =
  { id : Id.t
  ; row : int
  ; col : int
  ; finger : Finger.t
  ; hand : Hand.t
  ; kc : Keycode.t
  ; x : float
  ; y : float
  }
[@@deriving sexp, bin_io, compare, equal]

val make
  :  Id.t
  -> x:(Id.t -> row:int -> col:int -> float)
  -> y:(Id.t -> row:int -> col:int -> float)
  -> t

val dist : t -> t -> float
val bigram : t -> t -> string
