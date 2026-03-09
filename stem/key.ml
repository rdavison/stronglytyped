open! Import

module Id = struct
  module T = struct
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
  end

  include T
  include Comparable.Make_binable (T)

  let default_kc (x : t) =
    match x with
    | `ESC -> `Legend "esc"
    | `F1 -> `Legend "F1"
    | `F2 -> `Legend "F2"
    | `F3 -> `Legend "F3"
    | `F4 -> `Legend "F4"
    | `F5 -> `Legend "F5"
    | `F6 -> `Legend "F6"
    | `F7 -> `Legend "F7"
    | `F8 -> `Legend "F8"
    | `F9 -> `Legend "F9"
    | `F10 -> `Legend "F10"
    | `F11 -> `Legend "F11"
    | `F12 -> `Legend "F12"
    | `POWER -> `Power
    | `TILDE -> `Sym ('`', '~')
    | `_1 -> `Sym ('1', '!')
    | `_2 -> `Sym ('2', '@')
    | `_3 -> `Sym ('3', '#')
    | `_4 -> `Sym ('4', '$')
    | `_5 -> `Sym ('5', '%')
    | `_6 -> `Sym ('6', '^')
    | `_7 -> `Sym ('7', '&')
    | `_8 -> `Sym ('8', '*')
    | `_9 -> `Sym ('9', '(')
    | `_0 -> `Sym ('0', ')')
    | `HYPHEN -> `Sym ('-', '_')
    | `EQUAL -> `Sym ('=', '+')
    | `BACKSPACE -> `Legend "delete"
    | `TAB -> `Legend "tab"
    | `Q -> `Alpha 'q'
    | `W -> `Alpha 'w'
    | `E -> `Alpha 'e'
    | `R -> `Alpha 'r'
    | `T -> `Alpha 't'
    | `Y -> `Alpha 'y'
    | `U -> `Alpha 'u'
    | `I -> `Alpha 'i'
    | `O -> `Alpha 'o'
    | `P -> `Alpha 'p'
    | `OPEN_BRACKET -> `Sym ('[', '{')
    | `CLOSE_BRACKET -> `Sym (']', '}')
    | `BACKSLASH -> `Sym ('\\', '|')
    | `CAPSLOCK -> `Legend "caps"
    | `A -> `Alpha 'a'
    | `S -> `Alpha 's'
    | `D -> `Alpha 'd'
    | `F -> `Alpha 'f'
    | `G -> `Alpha 'g'
    | `H -> `Alpha 'h'
    | `J -> `Alpha 'j'
    | `K -> `Alpha 'k'
    | `L -> `Alpha 'l'
    | `SEMICOLON -> `Sym (';', ':')
    | `QUOTE -> `Sym ('\'', '"')
    | `ENTER -> `Legend "return"
    | `LEFT_SHIFT -> `Legend "shift"
    | `Z -> `Alpha 'z'
    | `X -> `Alpha 'x'
    | `C -> `Alpha 'c'
    | `V -> `Alpha 'v'
    | `B -> `Alpha 'b'
    | `N -> `Alpha 'n'
    | `M -> `Alpha 'm'
    | `COMMA -> `Sym (',', '<')
    | `PERIOD -> `Sym ('.', '>')
    | `SLASH -> `Sym ('/', '?')
    | `RIGHT_SHIFT -> `Legend "shift"
    | `FN -> `Legend "fn"
    | `LEFT_CTRL -> `Legend "^"
    | `LEFT_OPTION -> `Legend "⌥"
    | `LEFT_COMMAND -> `Legend "⌘"
    | `SPACE -> `Legend ""
    | `RIGHT_COMMAND -> `Legend "⌘"
    | `RIGHT_OPTION -> `Legend "⌥"
    | `LEFT -> `Legend "◀"
    | `DOWN -> `Legend "▼"
    | `UP -> `Legend "▲"
    | `RIGHT -> `Legend "▶"
  ;;

  let key_width (t : t) =
    match t with
    | `ESC -> 1.5
    | `F1 -> 1.0
    | `F2 -> 1.0
    | `F3 -> 1.0
    | `F4 -> 1.0
    | `F5 -> 1.0
    | `F6 -> 1.0
    | `F7 -> 1.0
    | `F8 -> 1.0
    | `F9 -> 1.0
    | `F10 -> 1.0
    | `F11 -> 1.0
    | `F12 -> 1.0
    | `POWER -> 1.0
    | `TILDE -> 1.0
    | `_1 -> 1.0
    | `_2 -> 1.0
    | `_3 -> 1.0
    | `_4 -> 1.0
    | `_5 -> 1.0
    | `_6 -> 1.0
    | `_7 -> 1.0
    | `_8 -> 1.0
    | `_9 -> 1.0
    | `_0 -> 1.0
    | `HYPHEN -> 1.0
    | `EQUAL -> 1.0
    | `BACKSPACE -> 1.5
    | `TAB -> 1.5
    | `Q -> 1.0
    | `W -> 1.0
    | `E -> 1.0
    | `R -> 1.0
    | `T -> 1.0
    | `Y -> 1.0
    | `U -> 1.0
    | `I -> 1.0
    | `O -> 1.0
    | `P -> 1.0
    | `OPEN_BRACKET -> 1.0
    | `CLOSE_BRACKET -> 1.0
    | `BACKSLASH -> 1.0
    | `CAPSLOCK -> 1.75
    | `A -> 1.0
    | `S -> 1.0
    | `D -> 1.0
    | `F -> 1.0
    | `G -> 1.0
    | `H -> 1.0
    | `J -> 1.0
    | `K -> 1.0
    | `L -> 1.0
    | `SEMICOLON -> 1.0
    | `QUOTE -> 1.0
    | `ENTER -> 1.75
    | `LEFT_SHIFT -> 2.25
    | `Z -> 1.0
    | `X -> 1.0
    | `C -> 1.0
    | `V -> 1.0
    | `B -> 1.0
    | `N -> 1.0
    | `M -> 1.0
    | `COMMA -> 1.0
    | `PERIOD -> 1.0
    | `SLASH -> 1.0
    | `RIGHT_SHIFT -> 2.25
    | `FN -> 1.0
    | `LEFT_CTRL -> 1.0
    | `LEFT_OPTION -> 1.0
    | `LEFT_COMMAND -> 1.25
    | `SPACE -> 5.0
    | `RIGHT_COMMAND -> 1.25
    | `RIGHT_OPTION -> 1.0
    | `LEFT -> 0.75
    | `DOWN -> 0.75
    | `UP -> 0.75
    | `RIGHT -> 0.75
  ;;

  let row = function
    | `Q -> 2
    | `W -> 2
    | `E -> 2
    | `R -> 2
    | `T -> 2
    | `Y -> 2
    | `U -> 2
    | `I -> 2
    | `O -> 2
    | `P -> 2
    | `A -> 1
    | `S -> 1
    | `D -> 1
    | `F -> 1
    | `G -> 1
    | `H -> 1
    | `J -> 1
    | `K -> 1
    | `L -> 1
    | `SEMICOLON -> 1
    | `Z -> 0
    | `X -> 0
    | `C -> 0
    | `V -> 0
    | `B -> 0
    | `N -> 0
    | `M -> 0
    | `COMMA -> 0
    | `PERIOD -> 0
    | `SLASH -> 0
    | _ -> failwith "id not supported yet"
  ;;

  let col = function
    | `Q -> 0
    | `W -> 1
    | `E -> 2
    | `R -> 3
    | `T -> 4
    | `Y -> 5
    | `U -> 6
    | `I -> 7
    | `O -> 8
    | `P -> 9
    | `A -> 0
    | `S -> 1
    | `D -> 2
    | `F -> 3
    | `G -> 4
    | `H -> 5
    | `J -> 6
    | `K -> 7
    | `L -> 8
    | `SEMICOLON -> 9
    | `Z -> 0
    | `X -> 1
    | `C -> 2
    | `V -> 3
    | `B -> 4
    | `N -> 5
    | `M -> 6
    | `COMMA -> 7
    | `PERIOD -> 8
    | `SLASH -> 9
    | _ -> failwith "id not supported yet"
  ;;

  let hand : t -> Hand.t = function
    | `Q -> `l
    | `W -> `l
    | `E -> `l
    | `R -> `l
    | `T -> `l
    | `Y -> `r
    | `U -> `r
    | `I -> `r
    | `O -> `r
    | `P -> `r
    | `A -> `l
    | `S -> `l
    | `D -> `l
    | `F -> `l
    | `G -> `l
    | `H -> `r
    | `J -> `r
    | `K -> `r
    | `L -> `r
    | `SEMICOLON -> `r
    | `Z -> `l
    | `X -> `l
    | `C -> `l
    | `V -> `l
    | `B -> `l
    | `N -> `r
    | `M -> `r
    | `COMMA -> `r
    | `PERIOD -> `r
    | `SLASH -> `r
    | _ -> failwith "id not supported yet"
  ;;

  let finger : t -> Finger.t = function
    | `Q -> `p
    | `W -> `r
    | `E -> `m
    | `R -> `i
    | `T -> `i
    | `Y -> `i
    | `U -> `i
    | `I -> `m
    | `O -> `r
    | `P -> `p
    | `A -> `p
    | `S -> `r
    | `D -> `m
    | `F -> `i
    | `G -> `i
    | `H -> `i
    | `J -> `i
    | `K -> `m
    | `L -> `r
    | `SEMICOLON -> `p
    | `Z -> `p
    | `X -> `r
    | `C -> `m
    | `V -> `i
    | `B -> `i
    | `N -> `i
    | `M -> `i
    | `COMMA -> `m
    | `PERIOD -> `r
    | `SLASH -> `p
    | _ -> failwith "id not supported yet"
  ;;

  module Ansi = struct
    let x id ~row:_ ~col =
      let col = Float.of_int col in
      match id with
      | `Q | `W | `E | `R | `T | `Y | `U | `I | `O | `P -> col
      | `A | `S | `D | `F | `G | `H | `J | `K | `L | `SEMICOLON -> col +. 0.25
      | `Z | `X | `C | `V | `B | `N | `M | `COMMA | `PERIOD | `SLASH -> col +. 0.75
      | _ -> failwith "id not supported yet"
    ;;

    let y id ~row ~col:_ =
      match id with
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
      | `Z
      | `X
      | `C
      | `V
      | `B
      | `N
      | `M
      | `COMMA
      | `PERIOD
      | `SLASH -> Float.of_int row
      | _ -> failwith "id not supported yet"
    ;;
  end

  let all_var =
    [ `Q
    ; `W
    ; `E
    ; `R
    ; `T
    ; `Y
    ; `U
    ; `I
    ; `O
    ; `P
    ; `A
    ; `S
    ; `D
    ; `F
    ; `G
    ; `H
    ; `J
    ; `K
    ; `L
    ; `SEMICOLON
    ; `Z
    ; `X
    ; `C
    ; `V
    ; `B
    ; `N
    ; `M
    ; `COMMA
    ; `PERIOD
    ; `SLASH
    ]
  ;;

  let var_arr = List.to_array all_var
  let var_length = Array.length var_arr

  let rand2 () =
    let f () = Random.int var_length in
    let a = f () in
    let b =
      let rec loop () =
        let b = f () in
        if Int.equal a b then loop () else b
      in
      loop ()
    in
    var_arr.(a), var_arr.(b)
  ;;

  module Pair = struct
    module T = struct
      type nonrec t = t * t [@@deriving sexp, compare, equal]

      type nonrec comparator_witness =
        (comparator_witness, comparator_witness) Tuple2.comparator_witness

      let comparator = Tuple2.comparator comparator comparator
    end

    include T
    include Comparable.Make_using_comparator (T)

    let all = List.cartesian_product all all
  end
end

module T = struct
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
  [@@deriving sexp, bin_io, equal, compare]
end

include T
include Comparable.Make (T)

let make id ~x ~y =
  let row = Id.row id in
  let col = Id.col id in
  let finger = Id.finger id in
  let hand = Id.hand id in
  let kc = Id.default_kc id in
  let x = x id ~row ~col in
  let y = y id ~row ~col in
  { id; row; col; finger; hand; kc; x; y }
;;

let dist (a : t) (b : t) = sqrt (((b.x -. a.x) ** 2.) +. ((b.y -. a.y) ** 2.))

let bigram (a : t) (b : t) =
  [ a.kc; b.kc ] |> List.map ~f:Keycode.to_string_lower |> String.concat
;;

module Pair = struct
  module T = struct
    type nonrec t = t * t [@@deriving sexp, compare]

    type nonrec comparator_witness =
      (comparator_witness, comparator_witness) Tuple2.comparator_witness

    let comparator = Tuple2.comparator comparator comparator
  end

  include T
  include Comparable.Make_using_comparator (T)
end

module Action = struct
  type t = Set of Keycode.t [@@deriving sexp, equal]
end

let state_machine id graph =
  let state, f =
    Bonsai.state_machine
      ~default_model:(make id ~x:Id.Ansi.x ~y:Id.Ansi.y)
      ~apply_action:(fun _ model action ->
        match action with
        | Action.Set kc ->
          (* print_s ([%sexp_of: Id.t * Keycode.t] (model.id, kc)); *)
          { model with kc })
      graph
  in
  Bonsai.both state f
;;
