open! Import
open! Incr

type t = Key.t list

let table =
  let ks cols =
    List.concat_map cols ~f:(fun c ->
        [ Key.all.(index 0 c); Key.all.(index 1 c); Key.all.(index 2 c) ])
  in
  Hand.all
  |> List.map ~f:(fun hand ->
         let data =
           match hand with
           | `L -> ks [ 0; 1; 2; 3; 4 ]
           | `R -> ks [ 5; 6; 7; 8; 9 ]
         in
         hand, all data)
  |> Hand.Table.of_alist_exn
;;
