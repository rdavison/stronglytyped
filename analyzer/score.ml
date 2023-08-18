open! Import

module Ast = struct
  type t =
    | Var of string
    | Num of float
    | Prj of t * string
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Div of t * t
    | Pow of t * t
  [@@deriving sexp]

  type map =
    [ `Hand of float Hand.Map.t Incr.t
    | `Hf of float Hf.Map.t Incr.t
    ]

  let uniq_vars t =
    let rec loop acc t =
      match t with
      | Var var -> var :: acc
      | Num _ -> acc
      | Prj (t1, _) -> loop acc t1
      | Add (t1, t2) | Sub (t1, t2) | Mul (t1, t2) | Div (t1, t2) | Pow (t1, t2) ->
        let v1 = loop acc t1 in
        let v2 = loop acc t2 in
        v1 @ v2
    in
    List.dedup_and_sort (loop [] t) ~compare:String.compare
  ;;

  let all_vars =
    let hf name stat total = name, (`Hf stat, total) in
    let hand name stat total = name, (`Hand stat, total) in
    let open Stats in
    [ hf "sfb" sfb sfb_total
    ; hf "dsfb" dsfb dsfb_total
    ; hf "speed" speed speed_total
    ; hand "lsb" lsb lsb_total
    ; hf "keyfreq" keyfreq keyfreq_total
    ; hand "roll" roll roll_total
    ; hand "roll_top" roll_top roll_top_total
    ; hand "roll_middle" roll_middle roll_middle_total
    ; hand "roll_bottom" roll_bottom roll_bottom_total
    ; hand "roll_in" roll_in roll_in_total
    ; hand "roll_in_top" roll_in_top roll_in_top_total
    ; hand "roll_in_middle" roll_in_middle roll_in_middle_total
    ; hand "roll_in_bottom" roll_in_bottom roll_in_bottom_total
    ; hand "roll_out" roll_out roll_out_total
    ; hand "roll_out_top" roll_out_top roll_out_top_total
    ; hand "roll_out_middle" roll_out_middle roll_out_middle_total
    ; hand "roll_out_bottom" roll_out_bottom roll_out_bottom_total
    ; hand "dshrc" dshrc dshrc_total
    ; hand "dshrc_good" dshrc_good dshrc_good_total
    ; hand "dshrc_bad" dshrc_bad dshrc_bad_total
    ]
  ;;

  let stats_of_vars t =
    let hf, hand =
      List.map t ~f:(fun var ->
        let map, total = List.Assoc.find_exn all_vars ~equal:String.equal var in
        var, (map, total))
      |> List.partition_map ~f:(fun (name, (map, total)) ->
        match map with
        | `Hf map ->
          let incr =
            let%map.Incr map = map
            and total = total in
            name, (map, total)
          in
          Either.First incr
        | `Hand map ->
          let incr =
            let%map.Incr map = map
            and total = total in
            name, (map, total)
          in
          Second incr)
    in
    let%map.Incr hf = Incr.all hf
    and hand = Incr.all hand in
    hf, hand
  ;;

  let project var prj =
    match List.Assoc.find all_vars ~equal:String.equal var with
    | None -> Error (sprintf "unknown var %s" var)
    | Some res ->
      (match res with
       | `Hf incr_map, _total ->
         (match Hf.of_string prj with
          | None -> Error (sprintf "unknown field %s in var %s" prj var)
          | Some hf ->
            let incr =
              let%map.Incr map = incr_map in
              Map.find_exn map hf
            in
            Ok incr)
       | `Hand incr_map, _total ->
         (match Hand.of_char prj.[0] with
          | None -> Error (sprintf "unknown field %s in var %s" prj var)
          | Some hand ->
            let incr =
              let%map.Incr map = incr_map in
              Map.find_exn map hand
            in
            Ok incr))
  ;;

  let compile t =
    let rec math f t1 t2 =
      let%map.Result t1 = recurse (Ok t1)
      and t2 = recurse (Ok t2) in
      let%map.Incr t1 = t1
      and t2 = t2 in
      f t1 t2
    and recurse acc =
      match acc with
      | Error e -> Error e
      | Ok acc ->
        (match acc with
         | Var var ->
           (match List.Assoc.find all_vars ~equal:String.equal var with
            | Some (_map, total) -> Ok total
            | None -> Error (sprintf "unknown var: %s" var))
         | Prj (Var var, prj) -> project var prj
         | Prj _ -> Error "invalid projection of non-var term"
         | Add (t1, t2) -> math ( +. ) t1 t2
         | Sub (t1, t2) -> math ( -. ) t1 t2
         | Mul (t1, t2) -> math ( *. ) t1 t2
         | Div (t1, t2) ->
           let ( /. ) a b = if Float.equal b Float.zero then Float.infinity else a /. b in
           math ( /. ) t1 t2
         | Pow (t1, t2) -> math ( ** ) t1 t2
         | Num n -> Ok (Incr.return n))
    in
    recurse (Ok t)
  ;;

  let of_syntax s =
    let rec loop res =
      match res with
      | Error e -> Error e
      | Ok acc ->
        (match acc with
         | Sexp.Atom atom ->
           (match Stdlib.Float.of_string_opt atom with
            | Some num -> Ok (Num num)
            | None ->
              (match String.split atom ~on:'.' with
               | [] | [ "" ] -> Error "invalid syntax"
               | [ var ] -> Ok (Var var)
               | [ var; prj ] -> Ok (Prj (Var var, prj))
               | _ -> Error "invalid syntax"))
         (* ^ vs  * *)
         | List (t1 :: Atom "*" :: t2 :: Atom "^" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Mul (t1, Pow (t2, t3))
         | List (t1 :: Atom "^" :: t2 :: Atom "*" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Mul (Pow (t1, t2), t3)
         (* ^ vs  / *)
         | List (t1 :: Atom "/" :: t2 :: Atom "*" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Div (t1, Pow (t2, t3))
         | List (t1 :: Atom "^" :: t2 :: Atom "/" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Div (Pow (t1, t2), t3)
         (* + vs  ^ *)
         | List (t1 :: Atom "+" :: t2 :: Atom "^" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Add (t1, Pow (t2, t3))
         | List (t1 :: Atom "^" :: t2 :: Atom "+" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Add (Pow (t1, t2), t3)
         (* - vs  ^ *)
         | List (t1 :: Atom "-" :: t2 :: Atom "^" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Sub (t1, Pow (t2, t3))
         | List (t1 :: Atom "^" :: t2 :: Atom "-" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Sub (Pow (t1, t2), t3)
         (* + vs  * *)
         | List (t1 :: Atom "+" :: t2 :: Atom "*" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Add (t1, Mul (t2, t3))
         | List (t1 :: Atom "*" :: t2 :: Atom "+" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Add (Mul (t1, t2), t3)
         (* - vs  * *)
         | List (t1 :: Atom "-" :: t2 :: Atom "*" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Sub (t1, Mul (t2, t3))
         | List (t1 :: Atom "*" :: t2 :: Atom "-" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Sub (Mul (t1, t2), t3)
         (* + vs  / *)
         | List (t1 :: Atom "+" :: t2 :: Atom "/" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Add (t1, Div (t2, t3))
         | List (t1 :: Atom "/" :: t2 :: Atom "+" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Add (Div (t1, t2), t3)
         (* - vs  / *)
         | List (t1 :: Atom "-" :: t2 :: Atom "/" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Sub (t1, Div (t2, t3))
         | List (t1 :: Atom "/" :: t2 :: Atom "-" :: (_ :: _ as t3)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok t2)
           and t3 = loop (Ok (Sexp.List t3)) in
           Sub (Div (t1, t2), t3)
         | List (t1 :: Atom "+" :: (_ :: _ as t2)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok (Sexp.List t2)) in
           Add (t1, t2)
         | List (t1 :: Atom "-" :: (_ :: _ as t2)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok (Sexp.List t2)) in
           Sub (t1, t2)
         | List (t1 :: Atom "*" :: (_ :: _ as t2)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok (Sexp.List t2)) in
           Mul (t1, t2)
         | List (t1 :: Atom "/" :: (_ :: _ as t2)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok (Sexp.List t2)) in
           Div (t1, t2)
         | List (t1 :: Atom "^" :: (_ :: _ as t2)) ->
           let%map.Result t1 = loop (Ok t1)
           and t2 = loop (Ok (Sexp.List t2)) in
           Pow (t1, t2)
         | List [ (Atom _ as atom) ] -> loop (Ok atom)
         | List [ (List _ as list) ] -> loop (Ok list)
         | List _ as sexp -> Error (sprintf "invalid syntax: %s" (Sexp.to_string sexp)))
    in
    let sexp =
      try Ok (Sexp.of_string s) with
      | _ ->
        (try Ok (Sexp.of_string (sprintf "(%s)" s)) with
         | _ -> Error (sprintf "invalid syntax: %s" s))
    in
    loop sexp
  ;;

  let%expect_test "of_syntax" =
    let test s =
      let t = of_syntax s in
      match t with
      | Ok ok -> print_s (sexp_of_t ok)
      | Error e -> printf "Error: %s" e
    in
    test "";
    [%expect {| Error: invalid syntax: () |}];
    test "1";
    [%expect {| (Num 1) |}];
    test "()";
    [%expect {| Error: invalid syntax: () |}];
    test "(1)";
    [%expect {| (Num 1) |}];
    test "(1 +)";
    [%expect {| Error: invalid syntax: (1 +) |}];
    test "(+ 1)";
    [%expect {| Error: invalid syntax: (+ 1) |}];
    test "(-1)";
    [%expect {| (Num -1) |}];
    test "-1";
    [%expect {| (Num -1) |}];
    test "(1 + 1)";
    [%expect {| (Add (Num 1) (Num 1)) |}];
    test "(1 - 1)";
    [%expect {| (Sub (Num 1) (Num 1)) |}];
    test "(1 - -1)";
    [%expect {| (Sub (Num 1) (Num -1)) |}];
    test "(1 * 1)";
    [%expect {| (Mul (Num 1) (Num 1)) |}];
    test "(1 / 1)";
    [%expect {| (Div (Num 1) (Num 1)) |}];
    test "(1 ^ 1)";
    [%expect {| (Pow (Num 1) (Num 1)) |}];
    test "1 + 2";
    [%expect {| (Add (Num 1) (Num 2)) |}];
    test "1 + 2 + 3";
    [%expect {| (Add (Num 1) (Add (Num 2) (Num 3))) |}];
    test "1 + 2 * 3";
    [%expect {| (Add (Num 1) (Mul (Num 2) (Num 3))) |}];
    test "1 * 2 + 3";
    [%expect {| (Add (Mul (Num 1) (Num 2)) (Num 3)) |}];
    test "(((((ineahtsr)))))";
    [%expect {| (Var ineahtsr) |}];
    test "foo / (bar + baz)";
    [%expect {| (Div (Var foo) (Add (Var bar) (Var baz))) |}];
    test "sfb.lp - -1 * dsfb.ri";
    [%expect {| (Sub (Prj (Var sfb) lp) (Mul (Num -1) (Prj (Var dsfb) ri))) |}];
    test "2 ^ 3 + 1";
    [%expect {| (Add (Pow (Num 2) (Num 3)) (Num 1)) |}];
    test "-1 - -2 - -3";
    [%expect {| (Sub (Num -1) (Sub (Num -2) (Num -3))) |}];
    test "(-1) - (-2) - (-3)";
    [%expect {| (Sub (Num -1) (Sub (Num -2) (Num -3))) |}];
    test "2 ^ dof.smie / 2.4";
    [%expect {| (Div (Pow (Num 2) (Prj (Var dof) smie)) (Num 2.4)) |}];
    test "2 / 2 ^ 3";
    [%expect {| (Div (Num 2) (Pow (Num 2) (Num 3))) |}];
    test "1 + (2 + (incomplete)";
    [%expect {| Error: invalid syntax: 1 + (2 + (incomplete) |}]
  ;;
end

let ( < ) = Float.( < )

let sfb =
  let%bind.Incr conf = Config.Incr.C.sfb in
  Imap.mapi Stats.sfb ~f:(fun ~key ~data ->
    let _, finger = key in
    let c, w = Map.find_exn conf finger in
    let wv = data *. w in
    if wv < c then 0. else Float.abs (c -. wv))
;;

let sfb_total = Imap.sum sfb (module Float) ~f:Fn.id

let dsfb =
  let%bind.Incr conf = Config.Incr.C.dsfb in
  Imap.mapi Stats.dsfb ~f:(fun ~key ~data ->
    let _, finger = key in
    let c, w = Map.find_exn conf finger in
    let wv = data *. w in
    if wv < c then 0. else Float.abs (c -. wv))
;;

let dsfb_total = Imap.sum dsfb (module Float) ~f:Fn.id

let speed =
  let%bind.Incr conf = Config.Incr.C.speed in
  Imap.mapi Stats.speed ~f:(fun ~key ~data ->
    let _, finger = key in
    let c, w = Map.find_exn conf finger in
    let wv = data *. w in
    Float.abs (c -. wv))
;;

let speed_total = Imap.sum speed (module Float) ~f:Fn.id

let lsb =
  let%bind.Incr c, w = Config.Incr.C.lsb in
  Imap.mapi Stats.lsb ~f:(fun ~key:_ ~data ->
    let wv = data *. w in
    if wv < c then 0. else Float.abs (c -. wv))
;;

let lsb_total = Imap.sum lsb (module Float) ~f:Fn.id

let roll =
  let%bind.Incr c, w_in, w_out = Config.Incr.C.roll in
  let w = (w_in +. w_out) /. 2. in
  let%map.Incr res =
    Imap.mapi Stats.roll ~f:(fun ~key:_ ~data ->
      let wv = 1. -. (w *. data) in
      if wv < c then 0. else Float.abs (c -. wv))
  in
  res
;;

let roll_total =
  let%map.Incr sum = Imap.sum roll (module Float) ~f:(fun i -> 1. -. i) in
  1. -. sum
;;

let dshrc =
  let%bind.Incr c, w = Incr.return (0., 2.) in
  let%map.Incr res =
    let dshrc =
      let%map_open.Incr dshrc_good = Stats.dshrc_good
      and dshrc_bad = Stats.dshrc_bad in
      List.fold Hand.all ~init:Hand.Map.empty ~f:(fun map h ->
        let good = Map.find_exn dshrc_good h in
        let bad = Map.find_exn dshrc_bad h in
        Map.add_exn map ~key:h ~data:(good, bad))
    in
    Imap.mapi dshrc ~f:(fun ~key:_ ~data ->
      let good, bad = data in
      let wv = good +. (w *. bad) in
      if wv < c then 0. else Float.abs (c -. wv))
  in
  res
;;

let dshrc_total = Imap.sum dshrc (module Float) ~f:Fn.id
let _incr = Incr.sum_float [| dshrc_total; roll_total; lsb_total; speed_total |]

let invert f =
  let%map.Incr f = f in
  1. -. f
;;

let w w f =
  let%map.Incr f = f in
  w *. f
;;

let incr =
  Incr.sum_float
    [| Stats.speed_total |> w 2.
     ; Stats.lsb_total |> w 3.
     ; Stats.dshrc_bad_total
     ; Stats.roll_total |> invert
    |]
;;
