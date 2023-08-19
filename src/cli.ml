open! Import

let main ~corpus =
  let corpus = Corpus.load_corpus corpus in
  let layout = Layout.ortho42 () in
  let weights = { Score.default_weights with sfss = 0. } in
  let score, _layout = Annealing.run layout ~corpus ~weights in
  printf "%.4f" score
;;

let cmd =
  let param =
    let open Command.Let_syntax in
    let%map_open corpus = return "typeracer" in
    fun () -> main ~corpus
  in
  Command.basic ~summary:"Generate layouts" param
;;
