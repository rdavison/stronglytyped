open! Import
open! Stronglytyped_analyzer

let observer = Incr.observe Analysis.incr

let next () =
  Incr.stabilize ();
  Incr.Observer.value_exn observer
;;

let nil () =
  Root.scramble 30;
  next ()
;;