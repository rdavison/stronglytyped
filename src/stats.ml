open! Import
open! Incr

type t =
  { sfb : float Hf.Table.t
  ; weight : float Hf.Table.t
  }

let make sfb weight = { sfb; weight }

let incr : t Incr.t =
  let open Let_syntax in
  let%bind config = Config.incr in
  let sfb =
    Sfb.table ~bigrams:config.bigrams
    |> Hf.Table.to_alist
    |> List.map ~f:(fun (key, data) -> map data ~f:(fun data -> key, data))
    |> all
    |> map ~f:Hf.Table.of_alist_exn
  in
  let weight =
    Weight.table ~monograms:config.monograms
    |> Hf.Table.to_alist
    |> List.map ~f:(fun (key, data) -> map data ~f:(fun data -> key, data))
    |> all
    |> map ~f:Hf.Table.of_alist_exn
  in
  map2 sfb weight ~f:(fun sfb weight -> { sfb; weight })
;;
