open! Import

let freq1 c1 ~data =
  match c1 with
  | `Char c1 -> Hashtbl.find_or_add data c1 ~default:(fun () -> 0.)
;;

let freq2 (c1, c2) ~data =
  match c1, c2 with
  | `Char c1, `Char c2 ->
    Hashtbl.find_or_add data (String.of_char_list [ c1; c2 ]) ~default:(fun () -> 0.)
;;

let freq3 (c1, c2, c3) ~data =
  match c1, c2, c3 with
  | `Char c1, `Char c2, `Char c3 ->
    Hashtbl.find_or_add data (String.of_char_list [ c1; c2; c3 ]) ~default:(fun () -> 0.)
;;
