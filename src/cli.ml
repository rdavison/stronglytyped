open! Import

let main ~corpus =
  let corpus = Corpus.load_corpus corpus in
  let layout = Layout.ansi () in
  let config = Score.default_config in
  let final_score, stats, score, save_state = Annealing.run layout ~corpus ~config in
  Layout.load layout save_state;
  Incr.stabilize ();
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
    (Stats.sexp_of_t stats |> Sexp.to_string_hum)
    ([%sexp_of: Score.t Incr.t] score |> Sexp.to_string_hum)
    final_score
    (Layout.pretty_string layout)
;;

(* let brute_force = Brute_force.run layout ~corpus ~weights in
   printf "%.12f\n%s\n%!" brute_force.score brute_force.pretty *)

let cmd =
  let param =
    let open Command.Let_syntax in
    let%map_open corpus = return "typeracer" in
    fun () -> main ~corpus
  in
  Command.basic ~summary:"Generate layouts" param
;;
