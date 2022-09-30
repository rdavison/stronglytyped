open! Import
open! Incr

type t =
  { code : Code.t
  ; rc : int * int
  ; hand : Hand.t
  ; finger : Finger.t
  }
[@@deriving sexp, compare, hash, equal]

let make i c : t =
  let code = `Char c in
  let r, c = i / 10, i mod 10 in
  let hand = if c <= 4 then `L else `R in
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

let all = Array.mapi Root.all ~f:(fun i v -> map (Var.watch v) ~f:(make i))

let dist ?(stagger = Stagger.default) k1 k2 =
  let pr, pc = k1.rc in
  let qr, qc = k2.rc in
  let px = Float.of_int pc +. Stagger.row_offset stagger pr in
  let qx = Float.of_int qc +. Stagger.row_offset stagger qr in
  let x = qx -. px in
  let y = Float.of_int (qr - pr) in
  Float.sqrt ((x *. x) +. (y *. y))
;;
