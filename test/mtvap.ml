open! Import

let%expect_test "check mtvap stats" =
  Layout.set (`Name "mtvap");
  let layout_pretty = Incr.observe Root.layout_pretty in
  let stats = Incr.observe Stats.incr in
  Incr.stabilize ();
  let layout_pretty = Incr.Observer.value_exn layout_pretty in
  let stats = Incr.Observer.value_exn stats in
  let res =
    String.concat
      ~sep:"\n"
      [ layout_pretty; "---------------------"; Stats.to_string stats ]
  in
  print_endline res;
  [%expect
    {|
    y p o u j k d l c w
    i n e a , m h t s r
    q z ' . ; b f v g x
    ---------------------
    hand-finger
    ┌─────────────────────────────────────────────────────────────────────┐
    │         (total)    LP    LR     LM     LI     RI     RM    RR    RP │
    │    sfb     0.88  0.03  0.00   0.20   0.28   0.07   0.13  0.13  0.03 │
    │   dsfb     8.79  0.36  0.26   2.50   1.38   2.17   1.09  0.68  0.35 │
    │  speed    11.27  0.41  0.27   2.94   1.97   2.94   1.43  0.91  0.40 │
    │ weight    80.86  7.66  6.79  16.58  11.11  12.79  11.42  8.23  6.28 │
    └─────────────────────────────────────────────────────────────────────┘
    hand
    ┌───────────────────────────────────────────┐
    │                      (total)      L     R │
    │                 lsb     1.05   0.59  0.46 │
    │               dshrc    13.27  11.37  1.90 │
    │        (good) dshrc    12.02  10.54  1.49 │
    │         (bad) dshrc     1.24   0.84  0.41 │
    │                roll    12.53   7.83  4.70 │
    │           (in) roll     2.71   2.71  0.00 │
    │          (out) roll     9.82   5.12  4.70 │
    │    (top)       roll     2.75   2.25  0.50 │
    │ (middle)       roll     9.77   5.58  4.19 │
    │ (bottom)       roll     0.01   0.00  0.00 │
    │    (top)  (in) roll     0.30   0.30  0.00 │
    │ (middle)  (in) roll     2.41   2.41  0.00 │
    │ (bottom)  (in) roll     0.00   0.00  0.00 │
    │    (top) (out) roll     2.46   1.95  0.50 │
    │ (middle) (out) roll     7.36   3.17  4.19 │
    │ (bottom) (out) roll     0.01   0.00  0.00 │
    └───────────────────────────────────────────┘ |}]
;;

let%expect_test "keyset" =
  Layout.set (`Name "mtvap");
  let table =
    let keyset x = Keyset.(x |> hf |> incr) in
    let%map.Incr assoc =
      Incr.all
      @@ let%map.List hf = Hf.all in
         let%map.Incr keyset = keyset hf in
         hf, keyset
    in
    Hf.Map.of_alist_exn assoc
  in
  let table = Incr.observe table in
  Incr.stabilize ();
  let table = Incr.Observer.value_exn table in
  print_s ([%sexp_of: Key.t list Hf.Map.t] table);
  [%expect
    {|
    (((L I)
      (((code (Char u)) (rc (0 3)) (hand L) (finger I))
       ((code (Char a)) (rc (1 3)) (hand L) (finger I))
       ((code (Char .)) (rc (2 3)) (hand L) (finger I))
       ((code (Char j)) (rc (0 4)) (hand L) (finger I))
       ((code (Char ,)) (rc (1 4)) (hand L) (finger I))
       ((code (Char ";")) (rc (2 4)) (hand L) (finger I))))
     ((L M)
      (((code (Char o)) (rc (0 2)) (hand L) (finger M))
       ((code (Char e)) (rc (1 2)) (hand L) (finger M))
       ((code (Char ')) (rc (2 2)) (hand L) (finger M))))
     ((L P)
      (((code (Char y)) (rc (0 0)) (hand L) (finger P))
       ((code (Char i)) (rc (1 0)) (hand L) (finger P))
       ((code (Char q)) (rc (2 0)) (hand L) (finger P))))
     ((L R)
      (((code (Char p)) (rc (0 1)) (hand L) (finger R))
       ((code (Char n)) (rc (1 1)) (hand L) (finger R))
       ((code (Char z)) (rc (2 1)) (hand L) (finger R))))
     ((R I)
      (((code (Char k)) (rc (0 5)) (hand R) (finger I))
       ((code (Char m)) (rc (1 5)) (hand R) (finger I))
       ((code (Char b)) (rc (2 5)) (hand R) (finger I))
       ((code (Char d)) (rc (0 6)) (hand R) (finger I))
       ((code (Char h)) (rc (1 6)) (hand R) (finger I))
       ((code (Char f)) (rc (2 6)) (hand R) (finger I))))
     ((R M)
      (((code (Char l)) (rc (0 7)) (hand R) (finger M))
       ((code (Char t)) (rc (1 7)) (hand R) (finger M))
       ((code (Char v)) (rc (2 7)) (hand R) (finger M))))
     ((R P)
      (((code (Char w)) (rc (0 9)) (hand R) (finger P))
       ((code (Char r)) (rc (1 9)) (hand R) (finger P))
       ((code (Char x)) (rc (2 9)) (hand R) (finger P))))
     ((R R)
      (((code (Char c)) (rc (0 8)) (hand R) (finger R))
       ((code (Char s)) (rc (1 8)) (hand R) (finger R))
       ((code (Char g)) (rc (2 8)) (hand R) (finger R))))) |}]
;;

let test keyset =
  Layout.set (`Name "mtvap");
  let sexp_config = Sexp_pretty.Config.create ~new_line_separator:true ~color:false () in
  let pretty_string = Sexp_pretty.pretty_string sexp_config in
  let keyset = Incr.observe keyset in
  Incr.stabilize ();
  let keyset = Incr.Observer.value_exn keyset in
  let keyset =
    List.map keyset ~f:(fun ((k1 : Key.t), (k2 : Key.t)) -> k1.code, k2.code)
  in
  print_endline ([%sexp_of: (Code.t * Code.t) list] keyset |> pretty_string)
;;

let%expect_test "keyset pairs" =
  test Keyset.((`L, `P) |> hf |> pairs |> incr2);
  [%expect
    {|
    (((Char y) (Char y))
     ((Char y) (Char i))
     ((Char y) (Char q))
     ((Char i) (Char y))
     ((Char i) (Char i))
     ((Char i) (Char q))
     ((Char q) (Char y))
     ((Char q) (Char i))
     ((Char q) (Char q))) |}]
;;

let%expect_test "sfb/dsfb/speed keyset" =
  test (Stats.Internal.Sfb.keyset (`L, `P));
  [%expect
    {|
    (((Char y) (Char i))
     ((Char y) (Char q))
     ((Char i) (Char y))
     ((Char i) (Char q))
     ((Char q) (Char y))
     ((Char q) (Char i))) |}]
;;

let%expect_test "lsb keyset" =
  test (Stats.Internal.Lsb.keyset `L);
  [%expect
    {|
    (((Char o)   (Char j))
     ((Char o)   (Char ,))
     ((Char o)   (Char ";"))
     ((Char e)   (Char j))
     ((Char e)   (Char ,))
     ((Char e)   (Char ";"))
     ((Char ')   (Char j))
     ((Char ')   (Char ,))
     ((Char ')   (Char ";"))
     ((Char j)   (Char o))
     ((Char j)   (Char e))
     ((Char j)   (Char '))
     ((Char ,)   (Char o))
     ((Char ,)   (Char e))
     ((Char ,)   (Char '))
     ((Char ";") (Char o))
     ((Char ";") (Char e))
     ((Char ";") (Char '))) |}];
  test (Stats.Internal.Lsb.keyset `R);
  [%expect
    {|
    (((Char k) (Char l))
     ((Char k) (Char t))
     ((Char k) (Char v))
     ((Char m) (Char l))
     ((Char m) (Char t))
     ((Char m) (Char v))
     ((Char b) (Char l))
     ((Char b) (Char t))
     ((Char b) (Char v))
     ((Char l) (Char k))
     ((Char l) (Char m))
     ((Char l) (Char b))
     ((Char t) (Char k))
     ((Char t) (Char m))
     ((Char t) (Char b))
     ((Char v) (Char k))
     ((Char v) (Char m))
     ((Char v) (Char b))) |}]
;;

let%expect_test "roll keyset" =
  test (Stats.Internal.Roll.keyset (`L, 0));
  [%expect
    {|
    (((Char y) (Char p))
     ((Char y) (Char o))
     ((Char y) (Char u))
     ((Char y) (Char j))
     ((Char p) (Char y))
     ((Char p) (Char o))
     ((Char p) (Char u))
     ((Char p) (Char j))
     ((Char o) (Char y))
     ((Char o) (Char p))
     ((Char o) (Char u))
     ((Char o) (Char j))
     ((Char u) (Char y))
     ((Char u) (Char p))
     ((Char u) (Char o))
     ((Char j) (Char y))
     ((Char j) (Char p))
     ((Char j) (Char o))) |}]
;;

let%expect_test "dshrc keyset" =
  test (Stats.Internal.Dshrc.keyset `L);
  [%expect
    {|
    (((Char y) (Char p))
     ((Char y) (Char o))
     ((Char y) (Char u))
     ((Char y) (Char j))
     ((Char y) (Char n))
     ((Char y) (Char e))
     ((Char y) (Char a))
     ((Char y) (Char ,))
     ((Char y) (Char z))
     ((Char y) (Char '))
     ((Char y) (Char .))
     ((Char y) (Char ";"))
     ((Char p) (Char o))
     ((Char p) (Char u))
     ((Char p) (Char j))
     ((Char o) (Char j))
     ((Char o) (Char a))
     ((Char o) (Char ,))
     ((Char i) (Char u))
     ((Char n) (Char o))
     ((Char o) (Char .))
     ((Char o) (Char ";"))
     ((Char i) (Char j))
     ((Char n) (Char u))
     ((Char n) (Char j))
     ((Char i) (Char n))
     ((Char e) (Char u))
     ((Char e) (Char j))
     ((Char i) (Char p))
     ((Char p) (Char e))
     ((Char p) (Char a))
     ((Char p) (Char ,))
     ((Char q) (Char p))
     ((Char q) (Char o))
     ((Char q) (Char u))
     ((Char q) (Char j))
     ((Char q) (Char n))
     ((Char q) (Char e))
     ((Char p) (Char '))
     ((Char ') (Char a))
     ((Char ') (Char ,))
     ((Char p) (Char .))
     ((Char p) (Char ";"))
     ((Char o) (Char u))
     ((Char i) (Char o))
     ((Char i) (Char e))
     ((Char i) (Char a))
     ((Char i) (Char ,))
     ((Char i) (Char z))
     ((Char i) (Char '))
     ((Char i) (Char .))
     ((Char i) (Char ";"))
     ((Char n) (Char e))
     ((Char n) (Char a))
     ((Char n) (Char ,))
     ((Char n) (Char '))
     ((Char n) (Char .))
     ((Char n) (Char ";"))
     ((Char e) (Char a))
     ((Char e) (Char ,))
     ((Char q) (Char a))
     ((Char q) (Char ,))
     ((Char z) (Char o))
     ((Char z) (Char u))
     ((Char z) (Char j))
     ((Char z) (Char e))
     ((Char z) (Char a))
     ((Char z) (Char ,))
     ((Char e) (Char .))
     ((Char e) (Char ";"))
     ((Char q) (Char z))
     ((Char ') (Char u))
     ((Char ') (Char j))
     ((Char q) (Char '))
     ((Char q) (Char .))
     ((Char q) (Char ";"))
     ((Char z) (Char '))
     ((Char z) (Char .))
     ((Char z) (Char ";"))
     ((Char ') (Char .))
     ((Char ') (Char ";"))) |}]
;;
