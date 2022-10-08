open! Import

type mode =
  | Iter of int
  | Analyze

let mode_v = Incr.Var.create (Iter 0)
let mode = Incr.Var.watch mode_v

let on_analysis f =
  let result =
    let%bind.Incr mode = mode in
    match mode with
    | Iter i ->
      let%map.Incr analysis = Analysis.incr in
      Some (i, analysis)
    | Analyze -> Incr.return None
  in
  Incr.observe result
  |> Incr.Observer.on_update_exn ~f:(function
         | Initialized None | Changed (_, None) | Invalidated -> ()
         | Initialized (Some (i, x)) -> f i x ~prev:None
         | Changed (None, Some (i, x)) -> f i x ~prev:None
         | Changed (Some (_, prev), Some (i, x)) -> f i x ~prev:(Some prev))
;;

let generate_layouts () =
  on_analysis (fun i (curr : Analysis.t) ~(prev : Analysis.t option) ->
      if Float.(curr.temperature > 1.)
      then Incr.Var.set mode_v (Iter (i + 1))
      else Incr.Var.set mode_v Analyze;
      let prev_score =
        Option.value_map prev ~default:Float.infinity ~f:(fun analysis -> analysis.score)
      in
      let curr_score = curr.score in
      let difference = Float.(curr_score - prev_score) in
      let best_score =
        match curr.best with
        | [] -> Float.infinity
        | (score, _) :: _ -> score
      in
      let () =
        if Float.(curr_score < best_score)
        then (
          Layout.set_best ((curr_score, curr.layout) :: curr.best);
          printf "Score: %.4f\n%s\n%!" curr_score curr.layout_pretty)
      in
      let () =
        if Float.(
             difference < 0.
             || exp (-1. *. (difference / curr.temperature)) > Random.float 1.)
        then Layout.set_next curr.layout
      in
      let () =
        Incr.Var.set Config.Var.temperature (curr.temperature *. curr.cooling_factor)
      in
      Root.scramble (Random.int 2 + 1))
;;

let reset () =
  Root.scramble 30;
  Incr.Var.set mode_v (Iter 0);
  Incr.Var.set Config.Var.temperature 100_000.
;;

let main () =
  Incr.save_dot_to_file "ypounercds.dot";
  Layout.set (`Name "qwerty");
  generate_layouts ();
  let mode = Incr.observe mode in
  Incr.stabilize ();
  let generating () =
    match Incr.Observer.value_exn mode with
    | Iter _ -> true
    | Analyze -> false
  in
  let counter = ref 1 in
  let attempts = 60 in
  let span, _ =
    time_it (fun () ->
        for _ = 1 to attempts do
          reset ();
          Incr.stabilize ();
          while generating () do
            incr counter;
            Incr.stabilize ()
          done
        done)
  in
  printf "Stabilized %d times. Time elapsed: %s\n" !counter (Time.Span.to_string_hum span)
;;

let command =
  Command.basic
    ~summary:"Ypou Nercds"
    (let%map_open.Command () = return () in
     fun () -> main ())
;;
