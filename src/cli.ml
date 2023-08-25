open! Import

let _main ~corpus =
  let corpus = Corpus.load_corpus corpus in
  let layout = Layout.ansi () in
  let score = Score.default_config in
  let final_score, stats, score, save_state = Annealing.run layout ~corpus ~score in
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

let main ~corpus =
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
