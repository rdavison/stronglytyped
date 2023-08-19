open! Core

let main () =
  let open Stronglytyped_analyzer in
  let corpus =
    let data =
      In_channel.read_all
        (match Sites.Sites.corpus with
         | [ path ] -> path ^/ "typeracer"
         | _ -> failwith "No path to corpus")
    in
    let sexp = Sexp.of_string data in
    Corpus.t_of_sexp sexp
  in
  let layout = Layout.ortho42 in
  let weights = { Score.default_weights with sfss = 0. } in
  let score, _layout = Annealing.run layout ~corpus ~weights in
  printf "%.4f" score
;;

let () =
  let param =
    let open Command.Let_syntax in
    let%map_open () = return () in
    fun () -> main ()
  in
  let command = Command.basic ~summary:"Generate layouts" param in
  Command_unix.run command
;;
