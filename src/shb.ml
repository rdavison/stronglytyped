open! Import
open! Incr

type t = float

let make keys ~bigrams =
  List.fold keys ~init:0. ~f:(fun init (k1 : Key.t) ->
      List.fold keys ~init ~f:(fun acc k2 ->
          match Hand.equal k1.hand k2.hand with
          | false -> acc
          | true ->
            acc
            +.
            let c1, c2 = k1.Key.code, k2.Key.code in
            (match c1, c2 with
            | `Char c1, `Char c2 ->
              String.Table.find_or_add
                bigrams
                (String.of_char_list [ c1; c2 ])
                ~default:(fun () -> 0.))))
;;

let incr =
  let%bind.Incr bigrams = Corpus.bigrams in
  By_hand.table
  |> Hand.Table.map ~f:(map ~f:(make ~bigrams))
  |> Hand.Table.to_alist
  |> List.map ~f:(fun (key, data) -> map data ~f:(fun data -> key, data))
  |> all
  |> map ~f:Hand.Table.of_alist_exn
;;
