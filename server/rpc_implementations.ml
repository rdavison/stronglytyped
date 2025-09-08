open! Core
open! Async

let version =
  let version_tag = sprintf "%d" (Random.int Int.max_value) in
  let f (_conn : Rpc.Connection.t) () = return version_tag in
  Rpc.Rpc.implement Stronglytyped_rpc.Protocol.Version.t f
;;

let implementations =
  Rpc.Implementations.create_exn
    ~implementations:[ version ]
    ~on_unknown_rpc:`Continue
    ~on_exception:Log_on_background_exn
;;
