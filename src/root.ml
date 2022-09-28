open! Import
open! Incr

type t = char Var.t

let all =
  "qwertyuiopasdfghjkl;zxcvbnm,./"
  |> String.to_list
  |> List.map ~f:(fun c -> Var.create c)
  |> List.to_array
;;

let swap a b =
  let tmp = Var.value all.(a) in
  Var.set all.(a) (Var.value all.(b));
  Var.set all.(b) tmp
;;

let length = Array.length all
