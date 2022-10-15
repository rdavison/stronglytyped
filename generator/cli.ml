open! Import

let param =
  let%map_open.Command () = return () in
  fun () -> Cjalgorithm.start 6
;;
