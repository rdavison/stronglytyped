open! Import
open! Incr

type t = float

let make keys ~bigrams ~skipgrams =
  List.fold keys ~init:0. ~f:(fun init k1 ->
      List.fold keys ~init ~f:(fun acc k2 ->
          let dist = Key.dist k1 k2 in
          let sfb =
            let c1, c2 = k1.Key.code, k2.Key.code in
            match c1, c2 with
            | `Char c1, `Char c2 ->
              String.Table.find_or_add
                bigrams
                (String.of_char_list [ c1; c2 ])
                ~default:(fun () -> 0.)
          in
          let dsfb =
            let c1, c2 = k1.Key.code, k2.Key.code in
            match c1, c2 with
            | `Char c1, `Char c2 ->
              String.Table.find_or_add
                skipgrams
                (String.of_char_list [ c1; c2 ])
                ~default:(fun () -> 0.)
          in
          acc +. (dist *. (sfb +. dsfb))))
;;

let incr =
  Incr.bind2 Corpus.bigrams Corpus.skipgrams ~f:(fun bigrams skipgrams ->
      By_hf.table
      |> Hf.Table.map ~f:(map ~f:(make ~bigrams ~skipgrams))
      |> Hf.Table.to_alist
      |> List.map ~f:(fun (key, data) -> map data ~f:(fun data -> key, data))
      |> all
      |> map ~f:Hf.Table.of_alist_exn)
;;
