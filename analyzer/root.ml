open! Import

type key =
  { var : char Incr.Var.t
  ; finger : int
  ; hand : int
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
[@@deriving sexp_of]

type t = key [@@deriving sexp_of]

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
    let var = Incr.Var.create (code i) in
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
    { var
    ; finger
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
    })
;;

let ortho42 =
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
       fun i -> s.(i).[0])
    ~finger:
      (let s =
         parse
           {|
    0 0 1 2 3 3 4 4 5 6 7 7
    0 0 1 2 3 3 4 4 5 6 7 7
    0 0 1 2 3 3 4 4 5 6 7 7
          8 8 8 9 9 9
    0 0 1 2 3 3 4 4 5 6 7 7
    0 0 1 2 3 3 4 4 5 6 7 7
    0 0 1 2 3 3 4 4 5 6 7 7
          8 8 8 9 9 9
    0 0 1 2 3 3 4 4 5 6 7 7
    0 0 1 2 3 3 4 4 5 6 7 7
    0 0 1 2 3 3 4 4 5 6 7 7
          8 8 8 9 9 9
    |}
       in
       fun i -> Int.of_string s.(i))
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
       fun i -> Int.of_string s.(i))
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
      | 0 | 12 | 24 | 35 | 36 | 37 | 38 | 39 | 40 | 41 -> true
      | _ -> false)
    ~locked_to:(fun i ->
      let layer, offset = layer_offset i in
      match offset with
      | 0 | 12 | 24 | 35 | 36 | 37 | 38 | 39 | 40 | 41 -> tower i |> List.to_array
      | _ ->
        let alpha_pair x =
          match layer with
          | 0 -> Some [| x + keys |]
          | 1 -> Some [| x - keys |]
          | _ -> None
        in
        let pairing =
          if (offset >= 1 && offset < 12)
             || (offset >= 13 && offset < 24)
             || (offset >= 25 && offset < 35)
          then alpha_pair i
          else None
        in
        Option.value pairing ~default:[||])
;;

(* let all =
  String.make 30 '@'
  |> String.to_array
  |> Array.map ~f:(fun c ->
       let var = Incr.Var.create c in
       { var
       ; finger = 0
       ; hand = 0
       ; x = 0.
       ; y = 0.
       ; col = 0
       ; row = 0
       ; layer = 0
       ; layer_trigger = None
       ; modifier = false
       ; swappable = true
       ; locked_to = [||]
       })
;; *)

let swap ?on_swap layout a b =
  Option.iter on_swap ~f:(fun f -> f (a, b));
  let tmp = Incr.Var.value layout.(a).var in
  Incr.Var.set layout.(a).var (Incr.Var.value layout.(b).var);
  Incr.Var.set layout.(b).var tmp
;;

let scramble ?on_swap layout i =
  for _ = 1 to i do
    let i, j = Random.int2 30 in
    swap ?on_swap layout i j
  done
;;

let length layout = Array.length layout
let rebase layout s = String.iteri s ~f:(fun i c -> Incr.Var.set layout.(i).var c)

let bijection layout =
  layout
  |> Array.mapi ~f:(fun i k ->
    let%map.Incr k = Incr.Var.watch k.var in
    i, k)
  |> Array.to_list
  |> Incr.all
;;

let char_list layout =
  let%map.Incr bijection = bijection layout in
  List.map ~f:snd bijection
;;

let reverse_lookup_table layout =
  let%map.Incr bijection = bijection layout in
  bijection |> List.map ~f:(fun (a, b) -> b, a) |> Char.Map.of_alist_exn
;;

let layout layout =
  let%map.Incr char_list = char_list layout in
  String.of_char_list char_list
;;

let layout_pretty layout =
  let%map.Incr char_list = char_list layout in
  let buf = Buffer.create 128 in
  List.iteri char_list ~f:(fun i v ->
    Buffer.add_char buf v;
    if i <> length layout - 1
    then Buffer.add_char buf (if i mod 10 = 9 then '\n' else ' '));
  Buffer.contents buf
;;
