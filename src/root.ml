open! Import
open! Incr

type t = char Var.t array

let v =
  "qwertyuiopasdfghjkl;zxcvbnm,./"
  |> String.to_list
  |> List.map ~f:(fun c -> Var.create c)
  |> List.to_array
;;

let swap a b =
  let tmp = Var.value v.(a) in
  Var.set v.(a) (Var.value v.(b));
  Var.set v.(b) tmp
;;

let length = Array.length v
