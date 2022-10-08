open! Import

let%expect_test "check whorf stats" =
  Layout.set (`Name "whorf");
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
    f l h d m v w o u ,
    s r n t k g y a e i
    x j b z q p c ' ; .
    ---------------------
    hand-finger
    ┌─────────────────────────────────────────────────────────────────────┐
    │         (total)    LP    LR     LM     LI    RI     RM     RR    RP │
    │    sfb     0.30  0.01  0.08   0.02   0.02  0.06   0.04   0.08  0.01 │
    │   dsfb     7.87  0.31  0.44   1.19   1.87  1.21   1.40   0.79  0.67 │
    │  speed     9.32  0.33  0.55   1.38   2.14  1.67   1.65   0.90  0.71 │
    │ weight    80.86  6.47  7.73  10.89  13.13  9.54  13.00  12.15  7.95 │
    └─────────────────────────────────────────────────────────────────────┘
    hand
    ┌──────────────────────────────────────────┐
    │                      (total)     L     R │
    │                 lsb     3.38  0.53  2.84 │
    │               dshrc    13.29  4.32  8.97 │
    │        (good) dshrc     9.61  2.57  7.05 │
    │         (bad) dshrc     3.68  1.76  1.92 │
    │                roll     6.99  2.78  4.21 │
    │           (in) roll     1.02  1.02  0.00 │
    │          (out) roll     5.97  1.75  4.21 │
    │    (top)       roll     2.27  0.36  1.91 │
    │ (middle)       roll     4.68  2.41  2.27 │
    │ (bottom)       roll     0.04  0.01  0.03 │
    │    (top)  (in) roll     0.08  0.08  0.00 │
    │ (middle)  (in) roll     0.93  0.93  0.00 │
    │ (bottom)  (in) roll     0.01  0.01  0.00 │
    │    (top) (out) roll     2.19  0.28  1.91 │
    │ (middle) (out) roll     3.74  1.47  2.27 │
    │ (bottom) (out) roll     0.03  0.00  0.03 │
    └──────────────────────────────────────────┘ |}]
;;

let%expect_test "keyset" =
  Layout.set (`Name "whorf");
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
      (((code (Char d)) (rc (0 3)) (hand L) (finger I))
       ((code (Char t)) (rc (1 3)) (hand L) (finger I))
       ((code (Char z)) (rc (2 3)) (hand L) (finger I))
       ((code (Char m)) (rc (0 4)) (hand L) (finger I))
       ((code (Char k)) (rc (1 4)) (hand L) (finger I))
       ((code (Char q)) (rc (2 4)) (hand L) (finger I))))
     ((L M)
      (((code (Char h)) (rc (0 2)) (hand L) (finger M))
       ((code (Char n)) (rc (1 2)) (hand L) (finger M))
       ((code (Char b)) (rc (2 2)) (hand L) (finger M))))
     ((L P)
      (((code (Char f)) (rc (0 0)) (hand L) (finger P))
       ((code (Char s)) (rc (1 0)) (hand L) (finger P))
       ((code (Char x)) (rc (2 0)) (hand L) (finger P))))
     ((L R)
      (((code (Char l)) (rc (0 1)) (hand L) (finger R))
       ((code (Char r)) (rc (1 1)) (hand L) (finger R))
       ((code (Char j)) (rc (2 1)) (hand L) (finger R))))
     ((R I)
      (((code (Char v)) (rc (0 5)) (hand R) (finger I))
       ((code (Char g)) (rc (1 5)) (hand R) (finger I))
       ((code (Char p)) (rc (2 5)) (hand R) (finger I))
       ((code (Char w)) (rc (0 6)) (hand R) (finger I))
       ((code (Char y)) (rc (1 6)) (hand R) (finger I))
       ((code (Char c)) (rc (2 6)) (hand R) (finger I))))
     ((R M)
      (((code (Char o)) (rc (0 7)) (hand R) (finger M))
       ((code (Char a)) (rc (1 7)) (hand R) (finger M))
       ((code (Char ')) (rc (2 7)) (hand R) (finger M))))
     ((R P)
      (((code (Char ,)) (rc (0 9)) (hand R) (finger P))
       ((code (Char i)) (rc (1 9)) (hand R) (finger P))
       ((code (Char .)) (rc (2 9)) (hand R) (finger P))))
     ((R R)
      (((code (Char u)) (rc (0 8)) (hand R) (finger R))
       ((code (Char e)) (rc (1 8)) (hand R) (finger R))
       ((code (Char ";")) (rc (2 8)) (hand R) (finger R))))) |}]
;;

let test keyset =
  Layout.set (`Name "whorf");
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
    (((Char f) (Char f))
     ((Char f) (Char s))
     ((Char f) (Char x))
     ((Char s) (Char f))
     ((Char s) (Char s))
     ((Char s) (Char x))
     ((Char x) (Char f))
     ((Char x) (Char s))
     ((Char x) (Char x))) |}]
;;

let%expect_test "sfb/dsfb/speed keyset" =
  test (Stats.Internal.Sfb.keyset (`L, `P));
  [%expect
    {|
    (((Char f) (Char s))
     ((Char f) (Char x))
     ((Char s) (Char f))
     ((Char s) (Char x))
     ((Char x) (Char f))
     ((Char x) (Char s))) |}]
;;

let%expect_test "lsb keyset" =
  test (Stats.Internal.Lsb.keyset `L);
  [%expect
    {|
    (((Char h) (Char m))
     ((Char h) (Char k))
     ((Char h) (Char q))
     ((Char n) (Char m))
     ((Char n) (Char k))
     ((Char n) (Char q))
     ((Char b) (Char m))
     ((Char b) (Char k))
     ((Char b) (Char q))
     ((Char m) (Char h))
     ((Char m) (Char n))
     ((Char m) (Char b))
     ((Char k) (Char h))
     ((Char k) (Char n))
     ((Char k) (Char b))
     ((Char q) (Char h))
     ((Char q) (Char n))
     ((Char q) (Char b))) |}];
  test (Stats.Internal.Lsb.keyset `R);
  [%expect
    {|
    (((Char v) (Char o))
     ((Char v) (Char a))
     ((Char v) (Char '))
     ((Char g) (Char o))
     ((Char g) (Char a))
     ((Char g) (Char '))
     ((Char p) (Char o))
     ((Char p) (Char a))
     ((Char p) (Char '))
     ((Char o) (Char v))
     ((Char o) (Char g))
     ((Char o) (Char p))
     ((Char a) (Char v))
     ((Char a) (Char g))
     ((Char a) (Char p))
     ((Char ') (Char v))
     ((Char ') (Char g))
     ((Char ') (Char p))) |}]
;;

let%expect_test "roll keyset" =
  test (Stats.Internal.Roll.keyset (`L, 0));
  [%expect
    {|
    (((Char f) (Char l))
     ((Char f) (Char h))
     ((Char f) (Char d))
     ((Char f) (Char m))
     ((Char l) (Char f))
     ((Char l) (Char h))
     ((Char l) (Char d))
     ((Char l) (Char m))
     ((Char h) (Char f))
     ((Char h) (Char l))
     ((Char h) (Char d))
     ((Char h) (Char m))
     ((Char d) (Char f))
     ((Char d) (Char l))
     ((Char d) (Char h))
     ((Char m) (Char f))
     ((Char m) (Char l))
     ((Char m) (Char h))) |}]
;;

let%expect_test "dshrc keyset" =
  test (Stats.Internal.Dshrc.keyset `L);
  [%expect {|
    (((Char f) (Char l))
     ((Char f) (Char h))
     ((Char f) (Char d))
     ((Char f) (Char m))
     ((Char f) (Char r))
     ((Char f) (Char n))
     ((Char f) (Char t))
     ((Char f) (Char k))
     ((Char f) (Char j))
     ((Char f) (Char b))
     ((Char f) (Char z))
     ((Char f) (Char q))
     ((Char l) (Char h))
     ((Char l) (Char d))
     ((Char l) (Char m))
     ((Char h) (Char m))
     ((Char h) (Char t))
     ((Char h) (Char k))
     ((Char s) (Char d))
     ((Char r) (Char h))
     ((Char h) (Char z))
     ((Char h) (Char q))
     ((Char s) (Char m))
     ((Char r) (Char d))
     ((Char r) (Char m))
     ((Char s) (Char r))
     ((Char n) (Char d))
     ((Char n) (Char m))
     ((Char s) (Char l))
     ((Char l) (Char n))
     ((Char l) (Char t))
     ((Char l) (Char k))
     ((Char x) (Char l))
     ((Char x) (Char h))
     ((Char x) (Char d))
     ((Char x) (Char m))
     ((Char x) (Char r))
     ((Char x) (Char n))
     ((Char l) (Char b))
     ((Char b) (Char t))
     ((Char b) (Char k))
     ((Char l) (Char z))
     ((Char l) (Char q))
     ((Char h) (Char d))
     ((Char s) (Char h))
     ((Char s) (Char n))
     ((Char s) (Char t))
     ((Char s) (Char k))
     ((Char s) (Char j))
     ((Char s) (Char b))
     ((Char s) (Char z))
     ((Char s) (Char q))
     ((Char r) (Char n))
     ((Char r) (Char t))
     ((Char r) (Char k))
     ((Char r) (Char b))
     ((Char r) (Char z))
     ((Char r) (Char q))
     ((Char n) (Char t))
     ((Char n) (Char k))
     ((Char x) (Char t))
     ((Char x) (Char k))
     ((Char j) (Char h))
     ((Char j) (Char d))
     ((Char j) (Char m))
     ((Char j) (Char n))
     ((Char j) (Char t))
     ((Char j) (Char k))
     ((Char n) (Char z))
     ((Char n) (Char q))
     ((Char x) (Char j))
     ((Char b) (Char d))
     ((Char b) (Char m))
     ((Char x) (Char b))
     ((Char x) (Char z))
     ((Char x) (Char q))
     ((Char j) (Char b))
     ((Char j) (Char z))
     ((Char j) (Char q))
     ((Char b) (Char z))
     ((Char b) (Char q))) |}]
;;
