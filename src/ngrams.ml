open! Import

module T2 = struct
  let freq (k1, k2) ~data =
    let c1, c2 = k1.Key.code, k2.Key.code in
    match c1, c2 with
    | `Char c1, `Char c2 ->
      String.Table.find_or_add
        data
        (String.of_char_list [ c1; c2 ])
        ~default:(fun () -> 0.)
  ;;
end