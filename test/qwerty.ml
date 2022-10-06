open! Import

let%expect_test "check qwerty stats" =
  Root.apply_layout (`Name "qwerty");
  let layout_pretty = Incr.observe Root.layout_pretty in
  let sfb_total = Incr.observe Stats.sfb_total in
  let dsfb_total = Incr.observe Stats.dsfb_total in
  let speed_total = Incr.observe Stats.speed_total in
  let lsb_total = Incr.observe Stats.lsb_total in
  Incr.stabilize ();
  let layout_pretty = Incr.Observer.value_exn layout_pretty in
  let sfb_total = Incr.Observer.value_exn sfb_total in
  let dsfb_total = Incr.Observer.value_exn dsfb_total in
  let speed_total = Incr.Observer.value_exn speed_total in
  let lsb_total = Incr.Observer.value_exn lsb_total in
  let res =
    String.concat
      ~sep:"\n"
      [ layout_pretty
      ; "---------------------"
      ; sprintf "  Sfb (total): %.4f" sfb_total
      ; sprintf " Dsfb (total): %.4f" dsfb_total
      ; sprintf "Speed (total): %.4f" speed_total
      ; sprintf "  Lsb (total): %.4f" lsb_total
      ]
  in
  print_endline res;
  [%expect
    {|
    q w e r t y u i o p
    a s d f g h j k l ;
    z x c v b n m , . /
    ---------------------
      Sfb (total): 0.0552
     Dsfb (total): 0.1712
    Speed (total): 0.2119
      Lsb (total): 0.1272 |}]
;;
