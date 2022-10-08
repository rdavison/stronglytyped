open! Import

let%expect_test "check qwerty stats" =
  Layout.set (`Name "qwerty");
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
    q w e r t y u i o p
    a s d f g h j k l ;
    z x c v b n m , . /
    ---------------------
    hand-finger
    ┌─────────────────────────────────────────────────────────────────────┐
    │         (total)    LP    LR     LM     LI     RI    RM     RR    RP │
    │    sfb     3.77  0.02  0.05   1.33   0.80   0.85  0.21   0.50  0.00 │
    │   dsfb    11.51  0.03  0.35   1.68   4.00   3.81  0.45   1.19  0.00 │
    │  speed    21.19  0.05  0.42   4.23   6.48   6.92  0.97   2.11  0.00 │
    │ weight    80.27  6.14  6.72  14.32  16.94  16.43  7.44  10.96  1.32 │
    └─────────────────────────────────────────────────────────────────────┘
    hand
    ┌──────────────────────────────────────────┐
    │                      (total)     L     R │
    │                 lsb    10.37  4.16  6.22 │
    │               dshrc    11.74  7.71  4.03 │
    │        (good) dshrc     9.53  5.87  3.65 │
    │         (bad) dshrc     2.21  1.84  0.38 │
    │                roll     7.89  4.91  2.99 │
    │           (in) roll     2.30  2.30  0.00 │
    │          (out) roll     5.59  2.61  2.99 │
    │    (top)       roll     6.18  3.49  2.69 │
    │ (middle)       roll     1.45  1.40  0.05 │
    │ (bottom)       roll     0.26  0.01  0.25 │
    │    (top)  (in) roll     1.71  1.71  0.00 │
    │ (middle)  (in) roll     0.59  0.59  0.00 │
    │ (bottom)  (in) roll     0.00  0.00  0.00 │
    │    (top) (out) roll     4.47  1.78  2.69 │
    │ (middle) (out) roll     0.86  0.82  0.05 │
    │ (bottom) (out) roll     0.26  0.01  0.25 │
    └──────────────────────────────────────────┘ |}]
;;

let%expect_test "keyset" =
  Layout.set (`Name "qwerty");
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
      (((code (Char r)) (rc (0 3)) (hand L) (finger I))
       ((code (Char f)) (rc (1 3)) (hand L) (finger I))
       ((code (Char v)) (rc (2 3)) (hand L) (finger I))
       ((code (Char t)) (rc (0 4)) (hand L) (finger I))
       ((code (Char g)) (rc (1 4)) (hand L) (finger I))
       ((code (Char b)) (rc (2 4)) (hand L) (finger I))))
     ((L M)
      (((code (Char e)) (rc (0 2)) (hand L) (finger M))
       ((code (Char d)) (rc (1 2)) (hand L) (finger M))
       ((code (Char c)) (rc (2 2)) (hand L) (finger M))))
     ((L P)
      (((code (Char q)) (rc (0 0)) (hand L) (finger P))
       ((code (Char a)) (rc (1 0)) (hand L) (finger P))
       ((code (Char z)) (rc (2 0)) (hand L) (finger P))))
     ((L R)
      (((code (Char w)) (rc (0 1)) (hand L) (finger R))
       ((code (Char s)) (rc (1 1)) (hand L) (finger R))
       ((code (Char x)) (rc (2 1)) (hand L) (finger R))))
     ((R I)
      (((code (Char y)) (rc (0 5)) (hand R) (finger I))
       ((code (Char h)) (rc (1 5)) (hand R) (finger I))
       ((code (Char n)) (rc (2 5)) (hand R) (finger I))
       ((code (Char u)) (rc (0 6)) (hand R) (finger I))
       ((code (Char j)) (rc (1 6)) (hand R) (finger I))
       ((code (Char m)) (rc (2 6)) (hand R) (finger I))))
     ((R M)
      (((code (Char i)) (rc (0 7)) (hand R) (finger M))
       ((code (Char k)) (rc (1 7)) (hand R) (finger M))
       ((code (Char ,)) (rc (2 7)) (hand R) (finger M))))
     ((R P)
      (((code (Char p)) (rc (0 9)) (hand R) (finger P))
       ((code (Char ";")) (rc (1 9)) (hand R) (finger P))
       ((code (Char /)) (rc (2 9)) (hand R) (finger P))))
     ((R R)
      (((code (Char o)) (rc (0 8)) (hand R) (finger R))
       ((code (Char l)) (rc (1 8)) (hand R) (finger R))
       ((code (Char .)) (rc (2 8)) (hand R) (finger R))))) |}]
;;

let test keyset =
  Layout.set (`Name "qwerty");
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
    (((Char q) (Char q))
     ((Char q) (Char a))
     ((Char q) (Char z))
     ((Char a) (Char q))
     ((Char a) (Char a))
     ((Char a) (Char z))
     ((Char z) (Char q))
     ((Char z) (Char a))
     ((Char z) (Char z))) |}]
;;

let%expect_test "sfb/dsfb/speed keyset" =
  test (Stats.Internal.Sfb.keyset (`L, `P));
  [%expect
    {|
    (((Char q) (Char a))
     ((Char q) (Char z))
     ((Char a) (Char q))
     ((Char a) (Char z))
     ((Char z) (Char q))
     ((Char z) (Char a))) |}]
;;

let%expect_test "lsb keyset" =
  test (Stats.Internal.Lsb.keyset `L);
  [%expect
    {|
    (((Char e) (Char t))
     ((Char e) (Char g))
     ((Char e) (Char b))
     ((Char d) (Char t))
     ((Char d) (Char g))
     ((Char d) (Char b))
     ((Char c) (Char t))
     ((Char c) (Char g))
     ((Char c) (Char b))
     ((Char t) (Char e))
     ((Char t) (Char d))
     ((Char t) (Char c))
     ((Char g) (Char e))
     ((Char g) (Char d))
     ((Char g) (Char c))
     ((Char b) (Char e))
     ((Char b) (Char d))
     ((Char b) (Char c))) |}];
  test (Stats.Internal.Lsb.keyset `R);
  [%expect
    {|
    (((Char y) (Char i))
     ((Char y) (Char k))
     ((Char y) (Char ,))
     ((Char h) (Char i))
     ((Char h) (Char k))
     ((Char h) (Char ,))
     ((Char n) (Char i))
     ((Char n) (Char k))
     ((Char n) (Char ,))
     ((Char i) (Char y))
     ((Char i) (Char h))
     ((Char i) (Char n))
     ((Char k) (Char y))
     ((Char k) (Char h))
     ((Char k) (Char n))
     ((Char ,) (Char y))
     ((Char ,) (Char h))
     ((Char ,) (Char n))) |}]
;;

let%expect_test "roll keyset" =
  test (Stats.Internal.Roll.keyset (`L, 0));
  [%expect
    {|
    (((Char q) (Char w))
     ((Char q) (Char e))
     ((Char q) (Char r))
     ((Char q) (Char t))
     ((Char w) (Char q))
     ((Char w) (Char e))
     ((Char w) (Char r))
     ((Char w) (Char t))
     ((Char e) (Char q))
     ((Char e) (Char w))
     ((Char e) (Char r))
     ((Char e) (Char t))
     ((Char r) (Char q))
     ((Char r) (Char w))
     ((Char r) (Char e))
     ((Char t) (Char q))
     ((Char t) (Char w))
     ((Char t) (Char e))) |}]
;;

let%expect_test "dshrc keyset" =
  test (Stats.Internal.Dshrc.keyset `L);
  [%expect
    {|
    (((Char q) (Char w))
     ((Char q) (Char e))
     ((Char q) (Char r))
     ((Char q) (Char t))
     ((Char q) (Char s))
     ((Char q) (Char d))
     ((Char q) (Char f))
     ((Char q) (Char g))
     ((Char q) (Char x))
     ((Char q) (Char c))
     ((Char q) (Char v))
     ((Char q) (Char b))
     ((Char w) (Char e))
     ((Char w) (Char r))
     ((Char w) (Char t))
     ((Char e) (Char t))
     ((Char e) (Char f))
     ((Char e) (Char g))
     ((Char a) (Char r))
     ((Char s) (Char e))
     ((Char e) (Char v))
     ((Char e) (Char b))
     ((Char a) (Char t))
     ((Char s) (Char r))
     ((Char s) (Char t))
     ((Char a) (Char s))
     ((Char d) (Char r))
     ((Char d) (Char t))
     ((Char a) (Char w))
     ((Char w) (Char d))
     ((Char w) (Char f))
     ((Char w) (Char g))
     ((Char z) (Char w))
     ((Char z) (Char e))
     ((Char z) (Char r))
     ((Char z) (Char t))
     ((Char z) (Char s))
     ((Char z) (Char d))
     ((Char w) (Char c))
     ((Char c) (Char f))
     ((Char c) (Char g))
     ((Char w) (Char v))
     ((Char w) (Char b))
     ((Char e) (Char r))
     ((Char a) (Char e))
     ((Char a) (Char d))
     ((Char a) (Char f))
     ((Char a) (Char g))
     ((Char a) (Char x))
     ((Char a) (Char c))
     ((Char a) (Char v))
     ((Char a) (Char b))
     ((Char s) (Char d))
     ((Char s) (Char f))
     ((Char s) (Char g))
     ((Char s) (Char c))
     ((Char s) (Char v))
     ((Char s) (Char b))
     ((Char d) (Char f))
     ((Char d) (Char g))
     ((Char z) (Char f))
     ((Char z) (Char g))
     ((Char x) (Char e))
     ((Char x) (Char r))
     ((Char x) (Char t))
     ((Char x) (Char d))
     ((Char x) (Char f))
     ((Char x) (Char g))
     ((Char d) (Char v))
     ((Char d) (Char b))
     ((Char z) (Char x))
     ((Char c) (Char r))
     ((Char c) (Char t))
     ((Char z) (Char c))
     ((Char z) (Char v))
     ((Char z) (Char b))
     ((Char x) (Char c))
     ((Char x) (Char v))
     ((Char x) (Char b))
     ((Char c) (Char v))
     ((Char c) (Char b))) |}]
;;
