open! Import

let command =
  Command.basic
    ~summary:"StronglyTyped Generator"
    (let%map_open.Command () = return () in
     fun () -> Cjalgorithm.start ())
;;
