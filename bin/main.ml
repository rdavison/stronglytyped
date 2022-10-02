open! Core

let () =
  if Sys_unix.file_exists_exn "corpus.txt"
  then (
    let s =
      In_channel.read_all "corpus.txt"
      |> Ypou.Corpus.of_string
      |> [%sexp_of: Ypou.Corpus.t]
      |> Sexp.to_string_mach
    in
    Out_channel.write_all "corpus.sexp" ~data:s)
;;

let () =
  let data =
    In_channel.read_all
      (match Sites.Sites.corpus with
      | [ path ] -> path ^/ "corpus.sexp"
      | _ -> failwith "No path to corpus")
  in
  Ypou.Incr.Var.set Ypou.Corpus.data_v data;
  Command_unix.run Ypou.Cli.command
;;
