open! Import
open! Incr

type t = char Var.t

let qwerty = "qwertyuiopasdfghjkl;zxcvbnm,./"
let all = qwerty |> String.to_list |> List.map ~f:(fun c -> Var.create c) |> List.to_array

let swap a b =
  let tmp = Var.value all.(a) in
  Var.set all.(a) (Var.value all.(b));
  Var.set all.(b) tmp
;;

let scramble i =
  for _ = 1 to i do
    let i, j = Random.int2 30 in
    swap i j
  done
;;

let length = Array.length all
let rebase s = String.iteri s ~f:(fun i c -> Var.set all.(i) c)
