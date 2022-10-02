open! Import
open! Incr

type t = float

let make keys ~skipgrams = Shb.make keys ~bigrams:skipgrams

let incr =
  let%bind.Incr skipgrams = Corpus.skipgrams in
  By_hand.table
  |> Hand.Table.map ~f:(map ~f:(make ~skipgrams))
  |> Hand.Table.to_alist
  |> List.map ~f:(fun (key, data) -> map data ~f:(fun data -> key, data))
  |> all
  |> map ~f:Hand.Table.of_alist_exn
;;
