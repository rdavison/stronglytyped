open! Import
open! Incr

type t = Key.t list Incr.t Finger.Table.t

let v =
  let ks cols =
    List.concat_map cols ~f:(fun c ->
        [ Key.all.(index 0 c); Key.all.(index 1 c); Key.all.(index 2 c) ])
  in
  Finger.all
  |> List.map ~f:(fun finger ->
         let data =
           match finger with
           | `P -> ks [ 0; 9 ]
           | `R -> ks [ 1; 8 ]
           | `M -> ks [ 2; 7 ]
           | `I -> ks [ 3; 4; 5; 6 ]
         in
         finger, all data)
  |> Finger.Table.of_alist_exn
;;
