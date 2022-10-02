open! Import
open! Incr

module T = struct
  type t =
    { code : Code.t
    ; rc : int * int
    ; hand : Hand.t
    ; finger : Finger.t
    }
  [@@deriving sexp, compare, hash, equal]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let make i c : t =
  let code = `Char c in
  let r, c = i / 10, i mod 10 in
  let hand = if Int.( <= ) c 4 then `L else `R in
  let finger =
    match c with
    | 0 | 9 -> `P
    | 1 | 8 -> `R
    | 2 | 7 -> `M
    | 3 | 4 | 5 | 6 -> `I
    | _ -> assert false
  in
  { code; rc = r, c; hand; finger }
;;

let all_arr_incr = Root.all |> Array.mapi ~f:(fun i v -> map (Var.watch v) ~f:(make i))
let all_incr_set = all_arr_incr |> Array.to_list |> Incr.all |> Incr.map ~f:Set.of_list
let all_incr_map = all_incr_set |> Imap.of_set

let dist ?(stagger = Stagger.default) k1 k2 =
  let pr, pc = k1.rc in
  let qr, qc = k2.rc in
  let px = Float.of_int pc +. Stagger.row_offset stagger pr in
  let qx = Float.of_int qc +. Stagger.row_offset stagger qr in
  let x = qx -. px in
  let y = Float.of_int (qr - pr) in
  Float.sqrt ((x *. x) +. (y *. y))
;;
