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
  ~swappable
  ~locked_to
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
        let swappable = swappable i ~code:(code i) ~modifier in
        let locked_to = locked_to i in
        { Key.finger
        ; hand
        ; x
        ; y
        ; col
        ; row
        ; layer
        ; layer_trigger
        ; modifier
        ; swappable
        ; locked_to
        }
      in
      let var = Incr.Var.create (code i) in
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
  { num_keys_per_layer; num_cols; num_rows; num_layers; keys }
;;

let ortho42 () =
  let layers = 3 in
  let keys = 42 in
  let layer_offset i = i / keys, i mod keys in
  let index layer offset = (layer * keys) + offset in
  let tower n =
    let layer, offset = layer_offset n in
    List.init layers ~f:(fun layer' -> layer', offset)
    |> List.filter_map ~f:(fun (layer', offset) ->
      if layer = layer' then None else Some (index layer' offset))
  in
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
    ~swappable:
      (let pinned = "1234567890bldwzfoujnrtsgyhaeixqmcvpkBLDWZFOUJNRTSGYHAEIXQMCVPK.'|" in
       fun i ~code ~modifier ->
         if modifier
         then false
         else (
           let _layer, offset = layer_offset i in
           match offset with
           | 0 | 12 | 24 | 35 | 36 | 37 | 38 | 39 | 40 | 41 -> false
           | _ ->
             let s = Code.to_string code in
             not (String.is_substring pinned ~substring:s)))
    ~locked_to:(fun i ->
      let layer, offset = layer_offset i in
      match offset with
      | 0 | 12 | 24 | 35 | 36 | 37 | 38 | 39 | 40 | 41 -> tower i
      | _ ->
        let alpha_pair x =
          match layer with
          | 0 -> Some [ x + keys ]
          | 1 -> Some [ x - keys ]
          | _ -> None
        in
        let pairing =
          if (offset >= 1 && offset < 12)
             || (offset >= 13 && offset < 24)
             || (offset >= 25 && offset < 35)
          then alpha_pair i
          else None
        in
        Option.value pairing ~default:[])
;;

let ansi () =
  let layers = 2 in
  let keys = 49 in
  let layer_offset i = i / keys, i mod keys in
  let index layer offset = (layer * keys) + offset in
  let tower n =
    let layer, offset = layer_offset n in
    List.init layers ~f:(fun layer' -> layer', offset)
    |> List.filter_map ~f:(fun (layer', offset) ->
      if layer = layer' then None else Some (index layer' offset))
  in
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
      N R T S G Y H A E I ,
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
    ~swappable:
      (let pinned = "1234567890bldwzfoujnrtsgyhaeixqmcvpkBLDWZFOUJNRTSGYHAEIXQMCVPK.'¥" in
       fun _i ~code ~modifier ->
         if modifier
         then false
         else (
           let s = Code.to_string code in
           not (String.is_substring pinned ~substring:s)))
    ~locked_to:(fun i ->
      let layer, offset = layer_offset i in
      match offset with
      | 37 | 48 -> tower i
      | _ ->
        let alpha_pair x =
          match layer with
          | 0 -> Some [ x + keys ]
          | 1 -> Some [ x - keys ]
          | _ -> None
        in
        let pairing =
          if (offset >= 13 && offset < 23)
             || (offset >= 26 && offset < 36)
             || (offset >= 38 && offset < 48)
          then alpha_pair i
          else None
        in
        Option.value pairing ~default:[])
;;

let swap ?on_swap (t : t) a b =
  Option.iter on_swap ~f:(fun f -> f (a, b));
  let _key_a, var_a = t.keys.(a) in
  let _key_b, var_b = t.keys.(b) in
  let vala = Incr.Var.value var_a in
  let valb = Incr.Var.value var_b in
  let tmp = vala in
  Incr.Var.set var_a valb;
  Incr.Var.set var_b tmp
;;

let scramble ?on_swap layout i =
  for _ = 1 to i do
    let i, j = Random.int2 30 in
    swap ?on_swap layout i j
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

let set (t : t) v =
  let layout =
    match v with
    | `Layout layout -> layout
    | `Name name ->
      List.find_map_exn all ~f:(fun (name', layout) ->
        if String.equal name name' then Some layout else None)
  in
  String.iteri layout ~f:(fun i c ->
    let _key, var = t.keys.(i) in
    Incr.Var.set var (`Char c))
;;

let valid_swaps (t : t) =
  let module Int2 = struct
    module T = struct
      type t = Int.t * Int.t [@@deriving sexp, compare, hash, equal]
    end

    include T
    include Comparable.Make (T)
  end
  in
  let acc, _seen =
    Array.foldi
      t.keys
      ~init:([], Set.empty (module Int2))
      ~f:(fun i init (k1, _v1) ->
        Array.foldi t.keys ~init ~f:(fun j (acc, seen) (k2, _v2) ->
          if i = j
          then acc, seen
          else if k1.swappable && k2.swappable && not (Set.mem seen (i, j))
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
          else acc, seen))
  in
  List.to_array acc
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
;;

let pretty_string (t : t) =
  let arr =
    Array.init t.num_layers ~f:(fun _ ->
      Array.init t.num_rows ~f:(fun _ -> Array.init t.num_cols ~f:(fun _ -> " ")))
  in
  Array.iter t.keys ~f:(fun (key, var) ->
    let code = Incr.Var.value var in
    arr.(key.layer).(key.row).(key.col) <- Code.to_string code);
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
