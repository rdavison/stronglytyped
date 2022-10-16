open! Import

type t = char Int.Map.t [@@deriving sexp, compare]

let of_string s : t =
  s |> String.to_list |> List.mapi ~f:(fun i c -> i, c) |> Int.Map.of_alist_exn
;;

let view keyboard =
  let open Incr_dom.Incr.Let_syntax in
  let open Incr_dom.Vdom in
  let%map top =
    Incr_map.filter_mapi keyboard ~f:(fun ~key:i ~data:c ->
        if i < 10 then Some c else None)
    |> Incr_map.map ~f:(fun c -> Node.td [ Node.text (Char.to_string c) ])
    |> Incr_dom.Incr.map ~f:(fun r -> Map.data r |> Node.tr)
  and middle =
    Incr_map.filter_mapi keyboard ~f:(fun ~key:i ~data:c ->
        if i >= 10 && i < 20 then Some c else None)
    |> Incr_map.map ~f:(fun c -> Node.td [ Node.text (Char.to_string c) ])
    |> Incr_dom.Incr.map ~f:(fun r -> Map.data r |> Node.tr)
  and bottom =
    Incr_map.filter_mapi keyboard ~f:(fun ~key:i ~data:c ->
        if i >= 20 && i < 30 then Some c else None)
    |> Incr_map.map ~f:(fun c -> Node.td [ Node.text (Char.to_string c) ])
    |> Incr_dom.Incr.map ~f:(fun r -> Map.data r |> Node.tr)
  in
  Node.table [ top; middle; bottom ]
;;