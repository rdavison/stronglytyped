open! Import

module T = struct
  type t =
    { finger : Finger.t
    ; hand : Hand.t
    ; x : float
    ; y : float
    ; col : int
    ; row : int
    ; layer : int
    ; layer_trigger : int option
    ; modifier : bool
    ; swappable : bool
    ; locked_to : int list
    }
  [@@deriving sexp, compare, hash, equal]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let dist k1 k2 ~stagger =
  let pr, pc = k1.row, k1.col in
  let qr, qc = k2.row, k2.col in
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

let slope rc1 rc2 ~stagger =
  let px, py = xy rc1 ~stagger in
  let qx, qy = xy rc2 ~stagger in
  let dx = qx -. px in
  let dy = qy -. py in
  dy /. dx
;;

let%expect_test "slope" =
  let test rc1 rc2 expect =
    let slope = slope rc1 rc2 ~stagger:Stagger.default in
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

let slope (k1 : t) (k2 : t) ~stagger = slope (k1.row, k1.col) (k2.row, k2.col) ~stagger

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
