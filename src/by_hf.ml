open! Import
open! Incr

type t = Key.t list Incr.t Hf.Table.t

let v =
  let ks cols =
    List.concat_map cols ~f:(fun c ->
        [ Key.all.(index 0 c); Key.all.(index 1 c); Key.all.(index 2 c) ])
  in
  Hf.all
  |> List.map ~f:(fun hf ->
         let data =
           match hf with
           | `L, `P -> ks [ 0 ]
           | `L, `R -> ks [ 1 ]
           | `L, `M -> ks [ 2 ]
           | `L, `I -> ks [ 3; 4 ]
           | `R, `I -> ks [ 5; 6 ]
           | `R, `M -> ks [ 7 ]
           | `R, `R -> ks [ 8 ]
           | `R, `P -> ks [ 9 ]
         in
         hf, all data)
  |> Hf.Table.of_alist_exn
;;
