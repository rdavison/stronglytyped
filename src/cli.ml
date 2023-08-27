open! Import

let main pool ~corpus =
  let res =
    List.init
      (Domainslib.Task.get_num_domains pool - 1)
      ~f:(fun _ ->
        Domainslib.Task.async pool (fun () ->
          let module Incr = Incremental.Make () in
          let module Layout = Layout.Make (Incr) in
          let module Stats = Stats.Make (Incr) (Layout) in
          let module Score = Score.Make (Incr) (Layout) (Stats) in
          let module Gen = Gen.Make (Incr) (Layout) (Stats) (Score) in
          let corpus = Corpus.load_corpus corpus in
          let layout = Layout.ansi () in
          let stats = Stats.make layout corpus in
          let score = Score.default_config stats in
          let stats = Stats.make layout corpus in
          let final_score_obs = Incr.observe (Score.final_sum score) in
          let gen = Gen.anneal layout ~final_score_obs in
          Layout.load layout gen.save_state;
          Incr.stabilize ();
          (* let gen = Gen.bruteforce layout ~final_score_obs in *)
          Layout.load layout gen.save_state;
          let pretty_stats_obs = Incr.observe (Stats.pretty_string stats) in
          Incr.stabilize ();
          let pretty_stats = Incr.Observer.value_exn pretty_stats_obs in
          let pretty_layout = Layout.pretty_string layout in
          ( pretty_stats
          , [%sexp_of: Score.t Incr.t] score |> Sexp.to_string_hum
          , gen.final_score
          , pretty_layout )))
  in
  List.map res ~f:(Domainslib.Task.await pool)
;;

let cmd =
  let param =
    let open Command.Let_syntax in
    let%map_open corpus = return "typeracer" in
    fun () ->
      let pool =
        Domainslib.Task.setup_pool
          ~num_domains:(Domain.recommended_domain_count () - 1)
          ()
      in
      let best = ref [] in
      while true do
        let res = Domainslib.Task.run pool (fun () -> main pool ~corpus) in
        best := res @ !best;
        !best
        |> List.sort ~compare:(fun (_, _, x, _) (_, _, y, _) -> Float.compare x y)
        |> (fun l -> List.take l 10)
        |> (fun l ->
             best := l;
             !best)
        |> List.rev
        |> List.iter ~f:(fun (stats, score, final_score, layout) ->
          printf
            "STATS\n\
             ==========\n\
             %s\n\n\
             SCORE\n\
             ==========\n\
             %s\n\n\
             FINAL SCORE\n\
             ==========\n\
             %.12f\n\n\
             LAYOUT\n\
             ==========\n\
             %s\n\
             %!"
            stats
            score
            final_score
            layout)
      done;
      Domainslib.Task.teardown_pool pool
  in
  Command.basic ~summary:"Generate layouts" param
;;
