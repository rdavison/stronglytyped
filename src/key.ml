open! Import

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

let all_arr_incr =
  Root.all |> Array.mapi ~f:(fun i v -> Incr.Var.watch v |> Incr.map ~f:(make i))
;;

let all_list_incr = all_arr_incr |> Array.to_list
let all_incr_set = all_arr_incr |> Array.to_list |> Incr.all |> Incr.map ~f:Set.of_list
let all_incr_map = all_incr_set |> Imap.of_set

let dist ?(stagger = Stagger.default) k1 k2 =
  let pr, pc = k1.rc in
  let qr, qc = k2.rc in
  let px = Float.of_int pc +. Stagger.row_offset stagger pr in
  let qx = Float.of_int qc +. Stagger.row_offset stagger qr in
  let dx = qx -. px in
  let dy = Float.of_int (qr - pr) in
  Float.sqrt ((dx *. dx) +. (dy *. dy))
;;

let direction (k1 : t) (k2 : t) =
  let ( - ) = Int.( - ) in
  let ( > ) = Int.( > ) in
  let f1 = Finger.to_int k1.finger in
  let f2 = Finger.to_int k2.finger in
  match k1.hand with
  | `L -> if f2 - f1 > 0 then `O else `I
  | `R -> if f2 - f2 > 0 then `I else `O
;;

let xy rc ~stagger =
  let r, c = rc in
  let x = Float.of_int c +. Stagger.row_offset stagger r in
  let y = Float.of_int ((-1 * r) + 2) in
  x, y
;;

let slope ?(stagger = Stagger.default) rc1 rc2 =
  let px, py = xy rc1 ~stagger in
  let qx, qy = xy rc2 ~stagger in
  let dx = qx -. px in
  let dy = qy -. py in
  dy /. dx
;;

let%expect_test "slope" =
  let test rc1 rc2 expect =
    let slope = slope rc1 rc2 in
    let actual = Float.compare slope 0. in
    let prefix = if Int.equal actual expect then "Pass" else "Fail" in
    let info =
      Info.create
        prefix
        (rc1, rc2, slope, actual, expect)
        [%sexp_of: (int * int) * (int * int) * float * int * int]
    in
    print_s (Info.sexp_of_t info)
  in
  test (0, 0) (2, 1) (-1);
  test (2, 0) (0, 1) 1;
  test (1, 3) (0, 2) (-1);
  [%expect
    {|
    (Pass ((0 0) (2 1) -1.3333333333333333 -1 -1))
    (Pass ((2 0) (0 1) 4 1 1))
    (Pass ((1 3) (0 2) -0.8 -1 -1)) |}]
;;

let slope ?stagger (k1 : t) (k2 : t) = slope ?stagger k1.rc k2.rc

module T2 = struct
  module T = struct
    type nonrec t = t * t [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module T3 = struct
  module T = struct
    type nonrec t = t * t * t [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end