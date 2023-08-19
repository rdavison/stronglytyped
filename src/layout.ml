open! Import

type t = (Key.t * Code.t Incr.Var.t) array [@@deriving sexp_of]

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
  Array.init n ~f:(fun i ->
    let key =
      let finger = finger i in
      let hand = hand i in
      let x, y = pos i in
      let col = col i in
      let row = row i in
      let layer = layer i in
      let layer_trigger = layer_trigger i in
      let modifier = modifier i in
      let swappable = swappable i in
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
;;

let ortho42 () =
  let layers = 2 in
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
    º b l d w z ' f o u j ,
    º n r t s g y h a e i ?
    º x q m c v p k . - = º
          º º º º º º 
    º B L D W Z : F O U J `
    º N R T S G Y H A E I !
    º X Q M C V P K < > | º
          º º º º º º
    º 1 2 3 4 5 6 7 8 9 0 ;
    º / " ( ) # º º º º º *
    º { } [ ] % \ + @ $ ^ º
          º º º º º º
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
    ~layer_trigger:(fun i ->
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
    ~swappable:(fun i ->
      let _layer, offset = layer_offset i in
      match offset with
      | 0 | 12 | 24 | 35 | 36 | 37 | 38 | 39 | 40 | 41 -> false
      | _ -> true)
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

let swap ?on_swap (t : t) a b =
  Option.iter on_swap ~f:(fun f -> f (a, b));
  let _key, var_a = t.(a) in
  let _key, var_b = t.(b) in
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

let length layout = Array.length layout

let rebase layout s =
  String.iteri s ~f:(fun i c ->
    let _key, var = layout.(i) in
    Incr.Var.set var (`Char c))
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
    let _key, var = t.(i) in
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
      t
      ~init:([], Set.empty (module Int2))
      ~f:(fun i init (k1, _v1) ->
        Array.foldi t ~init ~f:(fun j (acc, seen) (k2, _v2) ->
          if i = j
          then acc, seen
          else if k1.swappable && k2.swappable && not (Set.mem seen (i, j))
          then (
            let acc = (i, j) :: acc in
            let seen = Set.add seen (i, j) in
            let seen = Set.add seen (j, i) in
            acc, seen)
          else acc, seen))
  in
  List.to_array acc
;;
