open! Import
open! Async

let keyboard app =
  let f (_conn : Rpc.Connection.t) () =
    let%map (app : App.t) = app () in
    app.keyboard
  in
  Rpc.Rpc.implement Stem.Protocol.Keyboard.t f
;;

let gen app =
  let f (_conn : Rpc.Connection.t) () =
    let%map (app : App.t) = app () in
    app.keyboard, app.window
  in
  Rpc.Rpc.implement Stem.Protocol.Gen.t f
;;

let config app =
  let f (_conn : Rpc.Connection.t) (config : Stem.Config.t) =
    let%map (app : App.t) = app () in
    app.set_corpus config.corpus;
    app.clear_window ()
  in
  Rpc.Rpc.implement Stem.Protocol.Config.t f
;;

let version _app =
  let version_tag = sprintf "%d" (Random.int Int.max_value) in
  let f (_conn : Rpc.Connection.t) () = return version_tag in
  Rpc.Rpc.implement Stem.Protocol.Version.t f
;;

let optimizer app =
  let f (_conn : Rpc.Connection.t) optimizer =
    let%map (app : App.t) = app () in
    app.set_optimizer optimizer
  in
  Rpc.Rpc.implement Stem.Protocol.Optimizer.t f
;;

let dot app =
  let f (_conn : Rpc.Connection.t) data =
    let%map (app : App.t) = app () in
    app.set_dot data
  in
  Rpc.Rpc.implement Stem.Protocol.Dot.t f
;;

let implementations (app : unit -> App.t Deferred.t) =
  let implementations =
    let%map.List make = [ version; keyboard; gen; config; optimizer; dot ] in
    make app
  in
  Rpc.Implementations.create_exn
    ~implementations
    ~on_unknown_rpc:`Continue
    ~on_exception:Log_on_background_exn
;;
