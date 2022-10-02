open! Import

type t = char Incr.Var.t

let qwerty = "qwertyuiopasdfghjkl;zxcvbnm,./"

let all =
  qwerty |> String.to_list |> List.map ~f:(fun c -> Incr.Var.create c) |> List.to_array
;;

let swap a b =
  let tmp = Incr.Var.value all.(a) in
  Incr.Var.set all.(a) (Incr.Var.value all.(b));
  Incr.Var.set all.(b) tmp
;;

let scramble i =
  for _ = 1 to i do
    let i, j = Random.int2 30 in
    swap i j
  done
;;

let length = Array.length all
let rebase s = String.iteri s ~f:(fun i c -> Incr.Var.set all.(i) c)
