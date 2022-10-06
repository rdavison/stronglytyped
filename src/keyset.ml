open! Import

type t = (int * int) * Key.t Incr.t

let incr t = t |> List.map ~f:snd |> Incr.all

let incr2 t =
  t
  |> List.map ~f:(fun ((_, a), (_, b)) ->
         let%map_open.Incr a = a
         and b = b in
         a, b)
  |> Incr.all
;;

let pairs t =
  let%map_open.List k1 = t
  and k2 = t in
  k1, k2
;;

let dedup (t : t list) : t list =
  List.dedup_and_sort t ~compare:(fun (a1, _) (b1, _) -> [%compare: int * int] a1 b1)
;;

let dedup2 (t : (t * t) list) : (t * t) list =
  List.dedup_and_sort t ~compare:(fun ((a1, _), (a2, _)) ((b1, _), (b2, _)) ->
      [%compare: (int * int) * (int * int)] (a1, b1) (a2, b2))
;;

let unique_fingers2 (t : (t * t) list) : (t * t) list =
  List.filter t ~f:(fun ((a1, _), (a2, _)) ->
      let _r1, c1 = a1 in
      let _r2, c2 = a2 in
      match c1, c2 with
      | 3, 4 | 4, 3 | 5, 6 | 6, 5 -> false
      | i, j when i = j -> false
      | _ -> true)
;;

let symmetric2 (t : (t * t) list) : (t * t) list =
  (* first order the key pairs, and then dedup *)
  List.map t ~f:(fun ((((_, c1), _) as k1), (((_, c2), _) as k2)) ->
      if c1 <= c2 then k1, k2 else k2, k1)
  |> dedup2
;;

let make ~rows ~cols =
  let%map_open.List col = cols
  and row = rows in
  (row, col), Key.all_arr_incr.(index row col)
;;

let columns cols = make ~rows:[ 0; 1; 2 ] ~cols
let rows rows = make ~rows ~cols:[ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]

let hand h =
  columns
    (match h with
    | `L -> [ 0; 1; 2; 3; 4 ]
    | `R -> [ 9; 8; 7; 6; 5 ])
;;

let hf v =
  columns
    (match v with
    | `L, `P -> [ 0 ]
    | `L, `R -> [ 1 ]
    | `L, `M -> [ 2 ]
    | `L, `I -> [ 3; 4 ]
    | `R, `I -> [ 5; 6 ]
    | `R, `M -> [ 7 ]
    | `R, `R -> [ 8 ]
    | `R, `P -> [ 9 ])
;;

let hr v =
  match v with
  | `L, row -> make ~rows:[ row ] ~cols:[ 0; 1; 2; 3; 4 ]
  | `R, row -> make ~rows:[ row ] ~cols:[ 5; 6; 7; 8; 9 ]
;;

let finger v =
  columns
    (match v with
    | `P -> [ 0; 9 ]
    | `R -> [ 1; 8 ]
    | `M -> [ 2; 7 ]
    | `I -> [ 3; 4; 5; 6 ])
;;
