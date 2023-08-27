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

module Make (Incr : Incremental.S) = struct
  module Incr = Incr

  type var =
    { swappability : Swappability.t
    ; code : Code.t
    }
  [@@deriving sexp, hash, compare, equal]

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

  let init
    n
    ~code
    ~finger
    ~hand
    ~pos
    ~col
    ~row
    ~layer
    ~layer_trigger
    ~modifier
    ~swappability
    =
    let keys =
      Array.init n ~f:(fun i ->
        let key =
          let finger = finger i in
          let hand = hand i in
          let x, y = pos i in
          let col = col i in
          let row = row i in
          let layer = layer i in
          let layer_trigger = layer_trigger i ~hand in
          let modifier = modifier i in
          { Key.finger; hand; x; y; col; row; layer; layer_trigger; modifier }
        in
        let var = Incr.Var.create { swappability = swappability i; code = code i } in
        key, var)
    in
    let num_cols, num_rows, num_layers, num_keys_per_layer =
      let a, b, c, d =
        Array.fold
          keys
          ~init:(0, 0, 0, 0)
          ~f:(fun (max_col, max_row, max_layer, keys_per_layer) (key, _var) ->
            ( Int.max max_col key.col
            , Int.max max_row key.row
            , Int.max max_layer key.layer
            , keys_per_layer + if key.layer = 0 then 1 else 0 ))
      in
      a + 1, b + 1, c + 1, d
    in
    { num_keys_per_layer; num_cols; num_rows; num_layers; num_keys = n; keys }
  ;;

  let ortho42 () =
    let layers = 3 in
    let keys = 42 in
    let layer_offset i = i / keys, i mod keys in
    let parse s =
      s
      |> String.split_lines
      |> List.filter ~f:(function
        | "" -> false
        | _ -> true)
      |> List.concat_map ~f:(fun line ->
        line |> String.strip ~drop:(fun c -> Char.equal c ' ') |> String.split ~on:' ')
      |> List.to_array
    in
    init
      (layers * keys)
      ~code:
        (let s =
           parse
             {|
    | b l d w z ' f o u j !
    | n r t s g y h a e i ,
    | x q m c v p k . - = |
          | | | | | | 
    | B L D W Z : F O U J `
    | N R T S G Y H A E I ?
    | X Q M C V P K < > | |
          | | | | | | 
    | 1 2 3 4 5 6 7 8 9 0 ;
    | / " ( ) # | | | | | *
    | { } [ ] % \ + @ $ ^ |
          | | | | | | 
    |}
         in
         fun i -> `Char s.(i).[0])
      ~finger:
        (let s =
           parse
             {|
    0 0 1 2 3 3 3 3 2 1 0 0
    0 0 1 2 3 3 3 3 2 1 0 0
    0 0 1 2 3 3 3 3 2 1 0 0
          4 4 4 4 4 4
    0 0 1 2 3 3 3 3 2 1 0 0
    0 0 1 2 3 3 3 3 2 1 0 0
    0 0 1 2 3 3 3 3 2 1 0 0
          4 4 4 4 4 4
    0 0 1 2 3 3 3 3 2 1 0 0
    0 0 1 2 3 3 3 3 2 1 0 0
    0 0 1 2 3 3 3 3 2 1 0 0
          4 4 4 4 4 4
    |}
         in
         fun i -> Int.of_string s.(i) |> Finger.of_int)
      ~hand:
        (let s =
           parse
             {|
       1 1 1 1 1 1 2 2 2 2 2 2
       1 1 1 1 1 1 2 2 2 2 2 2
       1 1 1 1 1 1 2 2 2 2 2 2
             1 1 1 2 2 2
       1 1 1 1 1 1 2 2 2 2 2 2
       1 1 1 1 1 1 2 2 2 2 2 2
       1 1 1 1 1 1 2 2 2 2 2 2
             1 1 1 2 2 2
       1 1 1 1 1 1 2 2 2 2 2 2
       1 1 1 1 1 1 2 2 2 2 2 2
       1 1 1 1 1 1 2 2 2 2 2 2
             1 1 1 2 2 2
 |}
         in
         fun i -> Int.of_string s.(i) |> Hand.of_int)
      ~pos:(fun i ->
        let y =
          if i < 12
          then 3.
          else if i >= 12 && i < 24
          then 2.
          else if i >= 24 && i < 36
          then 1.
          else 0.
        in
        let x = Float.of_int ((i mod 12) + if i < 36 then 0 else 3) in
        x, y)
      ~col:
        (let s =
           parse
             {|
       0 1 2 3 4 5 6 7 8 9 10 11
       0 1 2 3 4 5 6 7 8 9 10 11
       0 1 2 3 4 5 6 7 8 9 10 11
             3 4 5 6 7 8
       0 1 2 3 4 5 6 7 8 9 10 11
       0 1 2 3 4 5 6 7 8 9 10 11
       0 1 2 3 4 5 6 7 8 9 10 11
             3 4 5 6 7 8
       0 1 2 3 4 5 6 7 8 9 10 11
       0 1 2 3 4 5 6 7 8 9 10 11
       0 1 2 3 4 5 6 7 8 9 10 11
             3 4 5 6 7 8
 |}
         in
         fun i -> Int.of_string s.(i))
      ~row:
        (let s =
           parse
             {|
       3 3 3 3 3 3 3 3 3 3 3 3
       2 2 2 2 2 2 2 2 2 2 2 2
       1 1 1 1 1 1 1 1 1 1 1 1
             0 0 0 0 0 0
       3 3 3 3 3 3 3 3 3 3 3 3
       2 2 2 2 2 2 2 2 2 2 2 2
       1 1 1 1 1 1 1 1 1 1 1 1
             0 0 0 0 0 0
       3 3 3 3 3 3 3 3 3 3 3 3
       2 2 2 2 2 2 2 2 2 2 2 2
       1 1 1 1 1 1 1 1 1 1 1 1
             0 0 0 0 0 0
 |}
         in
         fun i -> Int.of_string s.(i))
      ~layer:(fun i ->
        let layer, _offset = layer_offset i in
        layer)
      ~layer_trigger:(fun i ~hand:_ ->
        let layer, offset = layer_offset i in
        if layer = 0
        then None
        else if offset >= 36
        then None
        else (
          match offset with
          | 0 | 12 | 24 | 35 -> None
          | _ ->
            if layer = 1
            then Some 38
            else if layer = 2
            then Some 40
            else failwithf "BUG: No layer trigger for %d" i ()))
      ~modifier:(fun i ->
        let _layer, offset = layer_offset i in
        match offset with
        | 0 | 12 | 24 | 35 | 36 | 37 | 38 | 39 | 40 | 41 -> true
        | _ -> false)
      ~swappability:
        (let s =
           parse
             {|
      0 2 2 2 2 2 1 2 2 2 2 1
      0 2 2 2 2 2 2 2 2 2 2 1
      0 2 2 2 2 2 2 2 1 1 1 0
            0 0 0 0 0 0 
      0 2 2 2 2 2 1 2 2 2 2 1
      0 2 2 2 2 2 2 2 2 2 2 1
      0 2 2 2 2 2 2 2 1 1 1 0
            0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 1
      0 1 1 1 1 1 0 0 0 0 0 1
      0 1 1 1 1 1 1 1 1 1 1 0
            0 0 0 0 0 0
      |}
         in
         fun i -> Int.of_string s.(i) |> Swappability.of_int)
  ;;

  let ansi () =
    let layers = 2 in
    let keys = 49 in
    let layer_offset i = i / keys, i mod keys in
    let parse s =
      s
      |> String.split_lines
      |> List.filter ~f:(function
        | "" -> false
        | _ -> true)
      |> List.concat_map ~f:(fun line ->
        line |> String.strip ~drop:(fun c -> Char.equal c ' ') |> String.split ~on:' ')
      |> List.to_array
    in
    init
      (layers * keys)
      ~code:
        (let s =
           parse
             {|
    ` 1 2 3 4 5 6 7 8 9 0 [ ] 
      b l d w z ' f o u j ; = \
      n r t s g y h a e i ,
    ¥ x q m c v p k . - / ¥

    ~ ! @ # $ % ^ & * ( ) { } 
      B L D W Z _ F O U J : + |
      N R T S G Y H A E I ?
    ¥ X Q M C V P K > " < ¥
    |}
         in
         fun i -> `Char s.(i).[0])
      ~finger:
        (let s =
           parse
             {|
    0 0 1 2 3 3 3 3 2 1 0 0 0
      0 1 2 3 3 3 3 2 1 0 0 0 0
      0 1 2 3 3 3 3 2 1 0 0
    0 0 1 2 3 3 3 3 2 1 0 0

    0 0 1 2 3 3 3 3 2 1 0 0 0
      0 1 2 3 3 3 3 2 1 0 0 0 0
      0 1 2 3 3 3 3 2 1 0 0
    0 0 1 2 3 3 3 3 2 1 0 0
    |}
         in
         fun i -> Int.of_string s.(i) |> Finger.of_int)
      ~hand:
        (let s =
           parse
             {|
    1 1 1 1 1 1 2 2 2 2 2 2 2
      1 1 1 1 1 2 2 2 2 2 2 2 2
      1 1 1 1 1 2 2 2 2 2 2
    1 1 1 1 1 1 2 2 2 2 2 2

    1 1 1 1 1 1 2 2 2 2 2 2 2
      1 1 1 1 1 2 2 2 2 2 2 2 2
      1 1 1 1 1 2 2 2 2 2 2
    1 1 1 1 1 1 2 2 2 2 2 2
 |}
         in
         fun i -> Int.of_string s.(i) |> Hand.of_int)
      ~pos:(fun i ->
        let i = i mod keys in
        let y =
          if i < 13
          then 3.
          else if i >= 13 && i < 26
          then 2.
          else if i >= 26 && i < 37
          then 1.
          else 0.
        in
        let x =
          let i = Float.of_int i in
          let ( < ) = Float.( < ) in
          let ( >= ) = Float.( >= ) in
          if i < 13.
          then i +. 0.
          else if i >= 13. && i < 26.
          then i -. 13. +. 1.5
          else if i >= 26. && i < 37.
          then i -. 26. +. 1.75
          else i -. 37. +. 1.25
        in
        x, y)
      ~col:
        (let s =
           parse
             {|
    0 1 2 3 4 5 6 7 8 9 10 11 12
      1 2 3 4 5 6 7 8 9 10 11 12 13
      1 2 3 4 5 6 7 8 9 10 11
    0 1 2 3 4 5 6 7 8 9 10 11

    0 1 2 3 4 5 6 7 8 9 10 11 12
      1 2 3 4 5 6 7 8 9 10 11 12 13
      1 2 3 4 5 6 7 8 9 10 11
    0 1 2 3 4 5 6 7 8 9 10 11
 |}
         in
         fun i -> Int.of_string s.(i))
      ~row:
        (let s =
           parse
             {|
    3 3 3 3 3 3 3 3 3 3 3 3 3
      2 2 2 2 2 2 2 2 2 2 2 2 2
      1 1 1 1 1 1 1 1 1 1 1
    0 0 0 0 0 0 0 0 0 0 0 0

    3 3 3 3 3 3 3 3 3 3 3 3 3
      2 2 2 2 2 2 2 2 2 2 2 2 2
      1 1 1 1 1 1 1 1 1 1 1
    0 0 0 0 0 0 0 0 0 0 0 0
 |}
         in
         fun i -> Int.of_string s.(i))
      ~layer:(fun i ->
        let layer, _offset = layer_offset i in
        layer)
      ~layer_trigger:(fun i ~hand ->
        let layer, offset = layer_offset i in
        if layer = 0
        then None
        else (
          match offset with
          | 37 | 48 -> None
          | _ ->
            (match hand with
             | `L -> Some 48
             | `R -> Some 37)))
      ~modifier:(fun i ->
        let _layer, offset = layer_offset i in
        match offset with
        | 37 | 48 -> true
        | _ -> false)
      ~swappability:
        (let s =
           parse
             {|
      1 0 0 0 0 0 0 0 0 0 0 1 1 
        2 2 2 2 2 1 2 2 2 2 1 1 1
        2 2 2 2 2 2 2 2 2 2 1
      0 2 2 2 2 2 2 2 1 1 1 0

      1 1 1 1 1 1 1 1 1 1 1 1 1 
        2 2 2 2 2 1 2 2 2 2 1 1 1
        2 2 2 2 2 2 2 2 2 2 1
      0 2 2 2 2 2 2 2 1 1 1 0
      |}
         in
         fun i -> Int.of_string s.(i) |> Swappability.of_int)
  ;;

  let layer_offset (t : t) i = i / t.num_keys_per_layer, i mod t.num_keys_per_layer
  let index (t : t) ~layer ~offset = (layer * t.num_keys_per_layer) + offset

  let tower (t : t) idx =
    let _layer, offset = layer_offset t idx in
    List.init t.num_layers ~f:(fun layer -> index t ~layer ~offset)
  ;;

  let%expect_test "tower" =
    let layout = ortho42 () in
    let t1 = tower layout 1 in
    let t2 = tower layout 2 in
    let t3 = tower layout 11 in
    print_s ([%sexp_of: int list] t1);
    print_s ([%sexp_of: int list] t2);
    print_s ([%sexp_of: int list] t3);
    [%expect {|
    (1 43 85)
    (2 44 86)
    (11 53 95) |}]
  ;;

  (* let swap ?on_swap (t : t) a b =
     Option.iter on_swap ~f:(fun f -> f (a, b));
     let _key_a, var_a = t.keys.(a) in
     let _key_b, var_b = t.keys.(b) in
     let vala = Incr.Var.value var_a in
     let valb = Incr.Var.value var_b in
     let tmp = vala in
     Incr.Var.set var_a valb;
     Incr.Var.set var_b tmp
     ;; *)

  type swap = (int * var Incr.Var.t) * (int * var Incr.Var.t) [@@deriving sexp_of]

  let swaps (t : t) i j =
    let _k1, v1 = t.keys.(i) in
    let _k2, v2 = t.keys.(j) in
    let v1' = Incr.Var.value v1 in
    let v2' = Incr.Var.value v2 in
    if i = j
    then []
    else (
      match v1'.swappability, v2'.swappability with
      | Single, Single -> [ (i, v1), (j, v2) ]
      | Noswap, _ | _, Noswap -> []
      | Tower, _ | _, Tower ->
        let ts =
          List.zip_exn
            (tower t i |> List.map ~f:(fun k -> t.keys.(k)) |> List.map ~f:(fun s -> i, s))
            (tower t j |> List.map ~f:(fun k -> t.keys.(k)) |> List.map ~f:(fun s -> j, s))
        in
        (match
           List.for_all ts ~f:(fun ((_i, (_k1, v1)), (_j, (_k2, v2))) ->
             let v1' = Incr.Var.value v1 in
             let v2' = Incr.Var.value v2 in
             (* if String.equal (Code.to_string v1'.code) "b"
                then printf "%s\n\n" (sexp_of_swap ((_i, v1), (_j, v2)) |> Sexp.to_string_hum); *)
             match v1'.swappability, v2'.swappability with
             | (Single | Tower), (Single | Tower) -> true
             | Noswap, Noswap -> true
             | Noswap, _ | _, Noswap -> false)
         with
         | true ->
           List.filter_map ts ~f:(fun ((i, (_k1, v1)), (j, (_k2, v2))) ->
             let v1' = Incr.Var.value v1 in
             let v2' = Incr.Var.value v2 in
             match v1'.swappability, v2'.swappability with
             | (Single | Tower), (Single | Tower) -> Some ((i, v1), (j, v2))
             | Noswap, _ | _, Noswap -> None)
         | false -> []))
  ;;

  let swap (swaps : swap list) =
    List.iter swaps ~f:(fun ((_, s1), (_, s2)) ->
      let v1 = Incr.Var.value s1 in
      let v2 = Incr.Var.value s2 in
      Incr.Var.set s1 v2;
      Incr.Var.set s2 v1)
  ;;

  let scramble layout i =
    for _ = 1 to i do
      let i, j = Random.int2 30 in
      let swaps = swaps layout i j in
      swap swaps
    done
  ;;

  let all =
    [ "qwerty", "qwertyuiopasdfghjkl;zxcvbnm,./"
    ; "dvorak", "',.pyfgcrlaoeuidhtns;qjkxbmwvz"
    ; "mtvap", "ypoujkdlcwinea,mhtsrqz'.;bfvgx"
    ; "alphabet", "abcdefghijklmnopqrstuvwxyz'.,;"
    ; "whorf", "flhdmvwou,srntkgyaeixjbzqpc';."
    ]
  ;;

  (* let valid_swaps (t : t) =
     let module Int2 = struct
     module T = struct
     type t = Int.t * Int.t [@@deriving sexp, compare, hash, equal]
     end

     include T
     include Comparable.Make (T)
     end
     in
     let single_swaps, _seen =
     Array.foldi
     t.keys
     ~init:([], Set.empty (module Int2))
     ~f:(fun i init (k1, v1) ->
     let v1' = Incr.Var.value v1 in
     if Swappability.equal v1'.swappability Single
     then
     Array.foldi t.keys ~init ~f:(fun j (acc, seen) (k2, v2) ->
     let v2' = Incr.Var.value v2 in
     if Swappability.equal v2'.swappability Single
     then
     if i = j
     then acc, seen
     else if not (Set.mem seen (i, j))
     then (
     (* printf
     "%d\t%d\t%s\t%s\n"
     i
     j
     (Incr.Var.value v1 |> Code.to_string)
     (Incr.Var.value v2 |> Code.to_string); *)
     let acc = (i, j) :: acc in
     let seen = Set.add seen (i, j) in
     let seen = Set.add seen (j, i) in
     acc, seen)
     else acc, seen
     else acc, seen)
     else init)
     in
     List.to_array single_swaps
     ;;

     let valid_swaps2 (t : t) =
     let module Int22 = struct
     module T = struct
     type t = (Int.t * Int.t) * (Int.t * Int.t) [@@deriving sexp, compare, hash, equal]
     end

     include T
     include Comparable.Make (T)
     end
     in
     let valid_swaps = valid_swaps t in
     printf "Valid Swaps 1: %d\n%!" (Array.length valid_swaps);
     let acc, _seen =
     Array.fold
     valid_swaps
     ~init:([], Set.empty (module Int22))
     ~f:(fun init ((s1i, s1j) as s1) ->
     Array.fold valid_swaps ~init ~f:(fun (acc, seen) ((s2i, s2j) as s2) ->
     let uniq = Int.Set.of_array [| s1i; s1j; s2i; s2j |] in
     if Set.mem seen (s1, s2)
     then acc, seen
     else (
     match Set.length uniq with
     | 4 ->
     let a = s1i in
     let b = s1j in
     let c = s2i in
     let d = s2j in
     let seen = Set.add seen ((a, b), (c, d)) in
     let seen = Set.add seen ((a, b), (d, c)) in
     let seen = Set.add seen ((b, a), (c, d)) in
     let seen = Set.add seen ((b, a), (d, c)) in
     let seen = Set.add seen ((c, d), (a, b)) in
     let seen = Set.add seen ((c, d), (b, a)) in
     let seen = Set.add seen ((d, c), (a, b)) in
     let seen = Set.add seen ((d, c), (b, a)) in
     let acc = (s1, s2) :: acc in
     acc, seen
     | 3 ->
     let a = s1i in
     let b = s1j in
     let c = s2j in
     if s1i = s2i
     then (
     let seen = Set.add seen ((a, b), (a, c)) in
     let seen = Set.add seen ((a, b), (c, a)) in
     let seen = Set.add seen ((a, c), (b, c)) in
     let seen = Set.add seen ((a, c), (c, b)) in
     let seen = Set.add seen ((b, a), (a, c)) in
     let seen = Set.add seen ((b, a), (c, a)) in
     let seen = Set.add seen ((b, c), (a, b)) in
     let seen = Set.add seen ((b, c), (b, a)) in
     let seen = Set.add seen ((c, a), (b, c)) in
     let seen = Set.add seen ((c, a), (c, b)) in
     let seen = Set.add seen ((c, b), (a, b)) in
     let seen = Set.add seen ((c, b), (b, a)) in
     let acc = (s1, s2) :: acc in
     acc, seen)
     else if s1j = s2i
     then (
     let seen = Set.add seen ((a, b), (b, c)) in
     let seen = Set.add seen ((a, b), (c, b)) in
     let seen = Set.add seen ((a, c), (a, b)) in
     let seen = Set.add seen ((a, c), (b, a)) in
     let seen = Set.add seen ((b, a), (b, c)) in
     let seen = Set.add seen ((b, a), (c, b)) in
     let seen = Set.add seen ((b, c), (a, c)) in
     let seen = Set.add seen ((b, c), (c, a)) in
     let seen = Set.add seen ((c, a), (a, b)) in
     let seen = Set.add seen ((c, a), (b, a)) in
     let seen = Set.add seen ((c, b), (a, c)) in
     let seen = Set.add seen ((c, b), (c, a)) in
     let acc = (s1, s2) :: acc in
     acc, seen)
     else acc, seen
     | _ -> acc, seen)))
     in
     List.to_array acc
     ;; *)

  let pretty_string (t : t) =
    let arr =
      Array.init t.num_layers ~f:(fun _ ->
        Array.init t.num_rows ~f:(fun _ -> Array.init t.num_cols ~f:(fun _ -> " ")))
    in
    Array.iter t.keys ~f:(fun (key, var) ->
      let var' = Incr.Var.value var in
      arr.(key.layer).(key.row).(key.col) <- Code.to_string var'.code);
    arr
    |> Array.map ~f:(fun layer ->
      layer
      |> Array.rev
      |> Array.map ~f:(fun row -> row |> String.concat_array ~sep:" ")
      |> String.concat_array ~sep:"\n")
    |> String.concat_array ~sep:"\n"
  ;;

  let save (t : t) = Array.map t.keys ~f:(fun (_, var) -> Incr.Var.value var)

  let load (t : t) saved =
    Array.iteri saved ~f:(fun i code ->
      let _, var = t.keys.(i) in
      Incr.Var.set var code)
  ;;
end
