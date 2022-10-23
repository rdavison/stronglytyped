open! Import

type t =
  { next : unit -> Analysis.t Deferred.t
  ; nil : unit -> Analysis.t Deferred.t
  }

let make analysis =
  let observer = Incr.observe analysis in
  let next () =
    let%map () = Incr.stabilize () in
    Incr.Observer.value_exn observer
  in
  let nil () =
    Root.scramble 30;
    next ()
  in
  { next; nil }
;;
