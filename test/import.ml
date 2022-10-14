let () =
  let data =
    In_channel.read_all
      (match Sites.Sites.corpus with
      | [ path ] -> path ^/ "typeracer"
      | _ -> failwith "No path to corpus")
  in
  Incr.Var.set Corpus.data_v data
;;