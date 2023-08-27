open! Import

let main pool ~corpus =
  let res =
    List.init (Domainslib.Task.get_num_domains pool) ~f:(fun _ ->
      Domainslib.Task.async pool (fun () ->
        let module Incr = Incremental.Make () in
        let module Layout = Layout.Make (Incr) in
        let module Stats = Stats.Make (Incr) (Layout) in
        let module Score = Score.Make (Incr) (Layout) (Stats) in
        let module Annealing = Annealing.Make (Incr) (Layout) (Stats) (Score) in
        let corpus = Corpus.load_corpus corpus in
        let layout = Layout.ansi () in
        let score = Score.default_config in
        let final_score, stats, score, save_state = Annealing.run layout ~corpus ~score in
        Layout.load layout save_state;
        Incr.stabilize ();
        ( Stats.sexp_of_t stats |> Sexp.to_string_hum
        , [%sexp_of: Score.t Incr.t] score |> Sexp.to_string_hum
        , final_score
        , Layout.pretty_string layout )))
  in
  List.map res ~f:(Domainslib.Task.await pool)
;;

(* let _main ~corpus =
   let corpus = Corpus.load_corpus corpus in
   let layout = Layout.ansi () in
   let final_score, _stats, _score, save_state =
   Annealing.run layout ~corpus ~score:Score.default_config
   in
   let save_states = ref [ final_score, save_state ] in
   while true do
   save_states
   := List.sort !save_states ~compare:(fun (a, _) (b, _) -> Float.compare a b);
   save_states := List.take !save_states 8;
   let best_final_score, best_save_state = List.hd_exn !save_states in
   Layout.load layout best_save_state;
   Incr.stabilize ();
   printf
   "FINAL SCORE\n==========\n%.12f\n\nLAYOUT\n==========\n%s\n%!"
   best_final_score
   (Layout.pretty_string layout);
   save_states := List.rev !save_states;
   let last = List.hd_exn !save_states in
   let final_score, _stats, _score, save_state =
   Annealing.run layout ~corpus ~score:Score.default_config
   in
   if Float.( < ) final_score (fst last)
   then save_states := (final_score, save_state) :: !save_states
   done
   ;; *)

(* let brute_force = Brute_force.run layout ~corpus ~weights in
   printf "%.12f\n%s\n%!" brute_force.score brute_force.pretty *)

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
      Domainslib.Task.run pool (fun () -> main pool ~corpus)
      |> List.sort ~compare:(fun (_, _, x, _) (_, _, y, _) -> Float.compare x y)
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
          layout);
      Domainslib.Task.teardown_pool pool
  in
  Command.basic ~summary:"Generate layouts" param
;;
