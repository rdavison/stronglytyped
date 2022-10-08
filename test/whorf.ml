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
    │    sfb     2.06  0.25  0.66   0.08   0.20  0.17   0.25   0.39  0.06 │
    │   dsfb    13.48  0.66  0.78   1.81   2.87  1.45   2.44   2.32  1.15 │
    │  speed     9.32  0.33  0.55   1.38   2.14  1.67   1.65   0.90  0.71 │
    │ weight    80.86  6.47  7.73  10.89  13.13  9.54  13.00  12.15  7.95 │
    └─────────────────────────────────────────────────────────────────────┘
    hand
    ┌───────────────────────────────────────────┐
    │                      (total)     L      R │
    │                 lsb     3.44  0.55   2.89 │
    │               dshrc    26.58  8.64  17.94 │
    │        (good) dshrc    19.23  5.13  14.10 │
    │         (bad) dshrc     7.35  3.51   3.84 │
    │                roll     6.99  2.78   4.21 │
    │           (in) roll     1.02  1.02   0.00 │
    │          (out) roll     5.97  1.75   4.21 │
    │    (top)       roll     2.27  0.36   1.91 │
    │ (middle)       roll     4.68  2.41   2.27 │
    │ (bottom)       roll     0.04  0.01   0.03 │
    │    (top)  (in) roll     0.08  0.08   0.00 │
    │ (middle)  (in) roll     0.93  0.93   0.00 │
    │ (bottom)  (in) roll     0.01  0.01   0.00 │
    │    (top) (out) roll     2.19  0.28   1.91 │
    │ (middle) (out) roll     3.74  1.47   2.27 │
    │ (bottom) (out) roll     0.03  0.00   0.03 │
    └───────────────────────────────────────────┘ |}]
;;
