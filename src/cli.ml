open! Import

let main ~corpus =
  let corpus = Corpus.load_corpus corpus in
  let layout = Layout.ansi () in
  let config = Score.default_config in
  let score, save_state = Annealing.run layout ~corpus ~config in
  Layout.load layout save_state;
  Incr.stabilize ();
  printf "%.12f\n%s\n%!" score (Layout.pretty_string layout)
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
