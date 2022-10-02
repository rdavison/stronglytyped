open! Import

type 'a t = ((int * int) * 'a Incr.t) list

let incr t = List.map t ~f:snd |> Incr.all
let sort = List.sort ~compare:(fun (rc1, _) (rc2, _) -> [%compare: int * int] rc1 rc2)

let pairs t =
  let%map_open.List k1 = t
  and k2 = t in
  k1, k2
;;

let dedup t =
  List.dedup_and_sort t ~compare:(fun (rc1, _) (rc2, _) -> [%compare: int * int] rc1 rc2)
;;

let make rs cs =
  let%map_open.List col = cs
  and row = rs in
  (row, col), Key.all_arr_incr.(index row col)
;;

let columns l = make [ 0; 1; 2 ] l
let rows l = make l [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]

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
  | `L, row -> make [ row ] [ 0; 1; 2; 3; 4 ]
  | `R, row -> make [ row ] [ 5; 6; 7; 8; 9 ]
;;

let finger v =
  columns
    (match v with
    | `P -> [ 0; 9 ]
    | `R -> [ 1; 8 ]
    | `M -> [ 2; 7 ]
    | `I -> [ 3; 4; 5; 6 ])
;;
