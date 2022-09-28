open! Import
open! Incr

type t = Key.t list

let table =
  let open List.Let_syntax in
  let ks row cols =
    let%map c = cols in
    Key.all.(index row c)
  in
  Hr.Table.of_alist_exn
  @@ let%map h, r = Hr.all in
     let data =
       let ks = ks r in
       match h with
       | `L -> ks [ 0; 1; 2; 3; 4 ]
       | `R -> ks [ 5; 6; 7; 8; 9 ]
     in
     (h, r), all data
;;
