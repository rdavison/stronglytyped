let () =
  Incr_dom.Start_app.start
    (module Stronglytyped_web.App)
    ~initial_model:Stronglytyped_web.App.initial_model_exn
    ~bind_to_element_with_id:"app"
;;