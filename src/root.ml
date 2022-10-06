open! Import

type t = char Incr.Var.t

let default =
  List.find_map_exn layouts ~f:(fun (name, layout) ->
      if String.equal name "alphabet" then Some layout else None)
;;

let all =
  default |> String.to_list |> List.map ~f:(fun c -> Incr.Var.create c) |> List.to_array
;;

let apply_layout v =
  let layout =
    match v with
    | `Layout layout -> layout
    | `Name name ->
      List.find_map_exn layouts ~f:(fun (name', layout) ->
          if String.equal name name' then Some layout else None)
  in
  String.iteri layout ~f:(fun i c -> Incr.Var.set all.(i) c)
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
let char_list = all |> Array.map ~f:Incr.Var.watch |> Array.to_list |> Incr.all

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
