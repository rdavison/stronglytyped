open! Import

let freq1 k1 ~data =
  let c1 = k1.Key.code in
  match c1 with
  | `Char c1 -> Hashtbl.find_or_add data c1 ~default:(fun () -> 0.)
;;

let freq2 (k1, k2) ~data =
  let c1, c2 = k1.Key.code, k2.Key.code in
  match c1, c2 with
  | `Char c1, `Char c2 ->
    Hashtbl.find_or_add data (String.of_char_list [ c1; c2 ]) ~default:(fun () -> 0.)
;;

let freq3 (k1, k2, k3) ~data =
  let c1, c2, c3 = k1.Key.code, k2.Key.code, k3.Key.code in
  match c1, c2, c3 with
  | `Char c1, `Char c2, `Char c3 ->
    Hashtbl.find_or_add data (String.of_char_list [ c1; c2; c3 ]) ~default:(fun () -> 0.)
;;
