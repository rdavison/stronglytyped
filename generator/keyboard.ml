open! Import

let observer = Incr.observe Analysis.incr

let next () =
  let%map () = Incr.stabilize () in
  Incr.Observer.value_exn observer
;;

let nil () =
  Root.scramble 30;
  next ()
;;