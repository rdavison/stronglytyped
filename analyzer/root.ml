open! Import

type t = char Incr.Var.t

let all =
  String.make 30 '@'
  |> String.to_list
  |> List.map ~f:(fun c -> Incr.Var.create c)
  |> List.to_array
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

let bijection =
  all
  |> Array.mapi ~f:(fun i k ->
         let%map.Incr k = Incr.Var.watch k in
         i, k)
  |> Array.to_list
  |> Incr.all
;;

let char_list =
  let%map.Incr bijection = bijection in
  List.map ~f:snd bijection
;;

let reverse_lookup_table =
  let%map.Incr bijection = bijection in
  bijection |> List.map ~f:(fun (a, b) -> b, a) |> Char.Map.of_alist_exn
;;

let layout =
  let%map.Incr char_list = char_list in
  String.of_char_list char_list
;;

let layout_pretty =
  let%map.Incr char_list = char_list in
  let buf = Buffer.create 128 in
  List.iteri char_list ~f:(fun i v ->
      Buffer.add_char buf v;
      if i <> length - 1 then Buffer.add_char buf (if i mod 10 = 9 then '\n' else ' '));
  Buffer.contents buf
;;
