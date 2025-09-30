open! Import

type t = Key.Id.t list list [@@deriving sexp, equal, compare]

let ansi : t =
  [ [ `ESC; `F1; `F2; `F3; `F4; `F5; `F6; `F7; `F8; `F9; `F10; `F11; `F12; `POWER ]
  ; [ `TILDE
    ; `_1
    ; `_2
    ; `_3
    ; `_4
    ; `_5
    ; `_6
    ; `_7
    ; `_8
    ; `_9
    ; `_0
    ; `HYPHEN
    ; `EQUAL
    ; `BACKSPACE
    ]
  ; [ `TAB
    ; `Q
    ; `W
    ; `E
    ; `R
    ; `T
    ; `Y
    ; `U
    ; `I
    ; `O
    ; `P
    ; `OPEN_BRACKET
    ; `CLOSE_BRACKET
    ; `BACKSLASH
    ]
  ; [ `CAPSLOCK; `A; `S; `D; `F; `G; `H; `J; `K; `L; `SEMICOLON; `QUOTE; `ENTER ]
  ; [ `LEFT_SHIFT; `Z; `X; `C; `V; `B; `N; `M; `COMMA; `PERIOD; `SLASH; `RIGHT_SHIFT ]
  ; [ `FN
    ; `LEFT_CTRL
    ; `LEFT_OPTION
    ; `LEFT_COMMAND
    ; `SPACE
    ; `RIGHT_COMMAND
    ; `RIGHT_OPTION
    ; `LEFT
    ; `DOWN
    ; `UP
    ; `RIGHT
    ]
  ]
;;

let k10x3 : t =
  [ [ `Q; `W; `E; `R; `T; `Y; `U; `I; `O; `P ]
  ; [ `A; `S; `D; `F; `G; `H; `J; `K; `L; `SEMICOLON ]
  ; [ `Z; `X; `C; `V; `B; `N; `M; `COMMA; `PERIOD; `SLASH ]
  ]
;;
