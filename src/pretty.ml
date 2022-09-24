open! Import
open! Incr

type t = string

let v =
  all (Root.v |> Array.map ~f:Var.watch |> Array.to_list)
  |> map ~f:(fun lst ->
         let buf = Buffer.create 128 in
         List.iteri lst ~f:(fun i v ->
             Buffer.add_char buf v;
             if i <> Root.length - 1
             then Buffer.add_char buf (if i mod 10 = 9 then '\n' else ' '));
         Buffer.contents buf)
;;