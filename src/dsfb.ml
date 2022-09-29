open! Import
open! Incr

type t = float

let make keys ~skipgrams = Sfb.make keys ~bigrams:skipgrams

let incr =
  let%bind.Incr skipgrams = Config.skipgrams in
  By_hf.table
  |> Hf.Table.map ~f:(map ~f:(make ~skipgrams))
  |> Hf.Table.to_alist
  |> List.map ~f:(fun (key, data) -> map data ~f:(fun data -> key, data))
  |> all
  |> map ~f:Hf.Table.of_alist_exn
;;
