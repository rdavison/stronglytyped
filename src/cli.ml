open! Import

let base =
  ref
    (let b = Bytes.of_string Root.qwerty in
     for _ = 1 to 50 do
       let i, j = Random.int2 30 in
       let tmp = Bytes.get b i in
       Bytes.set b i (Bytes.get b j);
       Bytes.set b j tmp
     done;
     Bytes.to_string b)
;;

let stabilize () =
  Incr.stabilize ();
  Root.rebase !base
;;

let gen () =
  let observations = ref 0 in
  let observer = Incr.observe (Incr.both Config.kmax Config.neighbour) in
  stabilize ();
  let kmax, neighbour = Incr.Observer.value_exn observer in
  let () =
    Incr.observe Opt.anneal
    |> Incr.Observer.on_update_exn ~f:(function
           | Initialized (_, best :: _) | Changed (_, (_, best :: _)) ->
             incr observations;
             let { Analysis.score; layout; pretty } = best in
             base := layout;
             printf "Score: %.4f\n%s\n%!" score pretty
           | _ -> ())
  in
  stabilize ();
  Incr.save_dot_to_file "ypounercds.dot";
  let span, _ =
    time_it (fun () ->
        for k = 1 to kmax do
          let pct = Float.of_int k /. Float.of_int kmax in
          neighbour pct;
          Incr.Var.set Config.progress_v pct;
          stabilize ()
        done)
  in
  printf
    "Analyzed %s layouts, observed changes %s times. Time elapsed: %s\n"
    (Int.to_string_hum kmax)
    (Int.to_string_hum !observations)
    (Time.Span.to_string_hum span)
;;

let command =
  Command.basic
    ~summary:"Ypou Nercds"
    (let%map_open.Command () = return () in
     fun () -> gen ())
;;
