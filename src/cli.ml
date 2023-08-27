open! Import

module Worker = struct
  type t =
    { pretty_stats : string
    ; pretty_score : string
    ; pretty_layout : string
    ; final_score : float
    ; save_state : Layout.save_state
    }
  [@@deriving sexp]

  let to_string { pretty_stats; pretty_score; pretty_layout; final_score; save_state = _ }
    =
    sprintf
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
       %s"
      pretty_stats
      pretty_score
      final_score
      pretty_layout
  ;;

  let run ~corpus =
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
    let gen = Gen.bruteforce layout ~final_score_obs in
    Layout.load layout gen.save_state;
    let pretty_stats_obs = Incr.observe (Stats.pretty_string stats) in
    Incr.stabilize ();
    { pretty_stats = Incr.Observer.value_exn pretty_stats_obs
    ; pretty_score = [%sexp_of: Score.t Incr.t] score |> Sexp.to_string_hum
    ; pretty_layout = Layout.pretty_string layout
    ; final_score = gen.final_score
    ; save_state = gen.save_state
    }
  ;;
end

let main ~corpus =
  let module T = Domainslib.Task in
  let pool = T.setup_pool ~num_domains:(Domain.recommended_domain_count () - 1) () in
  let best = ref [] in
  while true do
    let res =
      T.run pool (fun () ->
        T.get_num_domains pool - 1
        |> List.init ~f:(fun _ -> T.async pool (fun () -> Worker.run ~corpus))
        |> List.map ~f:(T.await pool))
    in
    best := res @ !best;
    !best
    |> List.sort ~compare:(fun w1 w2 -> Float.compare w1.final_score w2.final_score)
    |> (fun l -> List.take l 10)
    |> (fun l ->
         best := l;
         !best)
    |> List.rev
    |> List.iter ~f:(Fn.compose (printf "%s\n%!") Worker.to_string)
  done;
  T.teardown_pool pool
;;

let cmd =
  let param =
    let open Command.Let_syntax in
    let%map_open corpus = return "typeracer" in
    fun () -> main ~corpus
  in
  Command.basic ~summary:"Generate layouts" param
;;
