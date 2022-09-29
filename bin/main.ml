open! Core

let () =
  let data =
    In_channel.read_all
      (match Sites.Sites.corpus with
      | [ path ] -> path ^/ "data.sexp"
      | _ -> failwith "No path to corpus")
  in
  Ypou.Corpus.set_data data;
  Command_unix.run Ypou.Cli.command
;;
