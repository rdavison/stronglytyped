open! Import

let main =
  Command.basic
    ~summary:"Ypou Nercds"
    (let%map_open.Command () = return () in
     fun () ->
       let observations = ref 0 in
       let span = ref None in
       let observer = Incr.observe (Incr.both Config.kmax Config.neighbour) in
       Incr.stabilize ();
       let kmax, neighbour = Incr.Observer.value_exn observer in
       let () =
         Incr.observe Opt.anneal
         |> Incr.Observer.on_update_exn ~f:(function
                | Initialized (best :: _) | Changed (_, best :: _) ->
                  incr observations;
                  let { Analysis.score; pretty } = best in
                  printf "Score: %.4f\n%s\n%!" score pretty
                | _ -> ())
       in
       let span', _ =
         time_it (fun () ->
             for k = 1 to kmax do
               let pct = Float.of_int k /. Float.of_int kmax in
               neighbour pct;
               Incr.stabilize ()
             done)
       in
       span := Some span';
       let span = Option.value_exn !span in
       printf
         "Analyzed %s layouts, observed changes %s times. Time elapsed: %s\n"
         (Int.to_string_hum kmax)
         (Int.to_string_hum !observations)
         (Time.Span.to_string_hum span))
;;
