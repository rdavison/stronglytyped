open! Import

let%expect_test "check qwerty stats" =
  Root.apply_layout (`Name "qwerty");
  let layout_pretty = Incr.observe Root.layout_pretty in
  let lsb_total = Incr.observe Stats.lsb_total in
  let stats = Incr.observe Stats.incr in
  Incr.stabilize ();
  let layout_pretty = Incr.Observer.value_exn layout_pretty in
  let lsb_total = Incr.Observer.value_exn lsb_total in
  let stats = Incr.Observer.value_exn stats in
  let res =
    String.concat
      ~sep:"\n"
      [ layout_pretty
      ; "---------------------"
      ; sprintf "  Lsb (total): %.4f" lsb_total
      ; Stats.to_string stats
      ]
  in
  print_endline res;
  [%expect
    {|
    q w e r t y u i o p
    a s d f g h j k l ;
    z x c v b n m , . /
    ---------------------
      Lsb (total): 0.1272
             (total)    LP      LR      LM      LI      RI      RM      RR      RP
        sfb  0.0552   0.0002  0.0023  0.0169  0.0107  0.0094  0.0021  0.0129  0.0007
       dsfb  0.1712   0.0054  0.0073  0.0333  0.0512  0.0460  0.0093  0.0185  0.0004
      speed  0.2119   0.0005  0.0042  0.0423  0.0648  0.0692  0.0097  0.0211  0.0000
    keyfreq  0.8027   0.0614  0.0672  0.1432  0.1694  0.1643  0.0744  0.1096  0.0132 |}]
;;
