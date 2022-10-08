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
    │    sfb     5.52  0.02  0.23   1.69   1.07   0.94  0.21   1.29  0.07 │
    │   dsfb    17.12  0.54  0.73   3.33   5.12   4.60  0.93   1.85  0.04 │
    │  speed    21.19  0.05  0.42   4.23   6.48   6.92  0.97   2.11  0.00 │
    │ weight    80.27  6.14  6.72  14.32  16.94  16.43  7.44  10.96  1.32 │
    └─────────────────────────────────────────────────────────────────────┘
    hand
    ┌───────────────────────────────────────────┐
    │                      (total)      L     R │
    │                 lsb    12.72   6.05  6.68 │
    │               dshrc    23.48  15.42  8.06 │
    │        (good) dshrc    19.05  11.74  7.31 │
    │         (bad) dshrc     4.43   3.68  0.75 │
    │                roll     7.89   4.91  2.99 │
    │           (in) roll     2.30   2.30  0.00 │
    │          (out) roll     5.59   2.61  2.99 │
    │    (top)       roll     6.18   3.49  2.69 │
    │ (middle)       roll     1.45   1.40  0.05 │
    │ (bottom)       roll     0.26   0.01  0.25 │
    │    (top)  (in) roll     1.71   1.71  0.00 │
    │ (middle)  (in) roll     0.59   0.59  0.00 │
    │ (bottom)  (in) roll     0.00   0.00  0.00 │
    │    (top) (out) roll     4.47   1.78  2.69 │
    │ (middle) (out) roll     0.86   0.82  0.05 │
    │ (bottom) (out) roll     0.26   0.01  0.25 │
    └───────────────────────────────────────────┘ |}]
;;
