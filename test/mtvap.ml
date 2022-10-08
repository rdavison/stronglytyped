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
    │    sfb     2.64  0.03  0.14   0.71   0.34   0.20   0.77  0.35  0.10 │
    │   dsfb    14.40  0.87  0.68   4.49   1.99   2.65   2.01  1.09  0.62 │
    │  speed    11.27  0.41  0.27   2.94   1.97   2.94   1.43  0.91  0.40 │
    │ weight    80.86  7.66  6.79  16.58  11.11  12.79  11.42  8.23  6.28 │
    └─────────────────────────────────────────────────────────────────────┘
    hand
    ┌───────────────────────────────────────────┐
    │                      (total)      L     R │
    │                 lsb     1.46   0.80  0.66 │
    │               dshrc    26.54  22.75  3.79 │
    │        (good) dshrc    24.05  21.07  2.98 │
    │         (bad) dshrc     2.49   1.67  0.82 │
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
