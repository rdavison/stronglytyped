open! Import
open! Incr

type t = Key.t * Key.t

let make k1 k2 = k1, k2

let all : t Incr.t array =
  Key.all
  |> Array.concat_map ~f:(fun k1 ->
         Key.all |> Array.map ~f:(fun k2 -> map2 k1 k2 ~f:make))
;;
