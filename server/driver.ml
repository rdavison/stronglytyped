open! Import
open! Async

let start (f : Bonsai.graph -> ((unit Ui_effect.t -> unit) -> App.t) Bonsai.t) =
  let instrumentation = Bonsai_driver.Instrumentation.default_for_test_handles () in
  let time_source = Bonsai.Time_source.create ~start:(Time_ns.now ()) in
  let driver = Bonsai_driver.create ~instrumentation ~time_source f in
  let schedule_event = Bonsai_driver.schedule_event driver in
  let bvar = Bvar.create () in
  let loop () =
    let%bind () = In_thread.run (fun () -> Bonsai_driver.flush driver) in
    let%bind make_app = In_thread.run (fun () -> Bonsai_driver.result driver) in
    let%bind () =
      In_thread.run (fun () -> Bvar.broadcast bvar (make_app schedule_event))
    in
    In_thread.run (fun () -> Bonsai_driver.trigger_lifecycles driver)
  in
  Deferred.forever () loop;
  fun () -> Bvar.wait bvar
;;
