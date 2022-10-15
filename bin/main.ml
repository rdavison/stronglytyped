open! Core

let () =
  if Sys_unix.file_exists_exn "corpus.txt"
  then (
    let s =
      In_channel.read_all "corpus.txt"
      |> Stronglytyped_analyzer.Corpus.of_string
      |> [%sexp_of: Stronglytyped_analyzer.Corpus.t]
      |> Sexp.to_string_mach
    in
    Out_channel.write_all "corpus.sexp" ~data:s)
;;

let generator =
  Async_command.async ~summary:"Stronglytyped Generator" Stronglytyped_generator.Cli.param
;;

let () =
  let data =
    In_channel.read_all
      (match Sites.Sites.corpus with
      | [ path ] -> path ^/ "typeracer"
      | _ -> failwith "No path to corpus")
  in
  Stronglytyped_analyzer.Incr.Var.set Stronglytyped_analyzer.Corpus.data_v data;
  let group =
    Command.group
      ~summary:"StronglyTyped is a keyboard layout analyzer and generator."
      [ "gen", generator ]
  in
  Command_unix.run group
;;
