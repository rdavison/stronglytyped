open! Core
open! Async

let keyboard_to_string keyboard =
  let arrangement = Analysis.Arrangement.k10x3 in
  let key id =
    let keycode =
      match
        let%map.Option key = Map.find keyboard id in
        key.Analysis.Key.kc
      with
      | None -> Analysis.Key.Id.default_kc id
      | Some kc -> kc
    in
    Analysis.Keycode.to_string_lower keycode
  in
  let row row = row |> List.map ~f:key |> String.concat ~sep:" " in
  let keyboard = arrangement |> List.map ~f:row |> String.concat ~sep:"\n" in
  keyboard
;;

let keyboard app =
  let f (_conn : Rpc.Connection.t) () =
    let%map (app : App.t) = app () in
    app.keyboard
  in
  Rpc.Rpc.implement Stronglytyped_rpc.Protocol.Keyboard.t f
;;

let version _app =
  let version_tag = sprintf "%d" (Random.int Int.max_value) in
  let f (_conn : Rpc.Connection.t) () = return version_tag in
  Rpc.Rpc.implement Stronglytyped_rpc.Protocol.Version.t f
;;

let implementations (app : unit -> App.t Deferred.t) =
  Rpc.Implementations.create_exn
    ~implementations:[ version app; keyboard app ]
    ~on_unknown_rpc:`Continue
    ~on_exception:Log_on_background_exn
;;
