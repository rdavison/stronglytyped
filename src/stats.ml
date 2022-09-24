open! Import
open! Incr

type t =
  { sfb : float Hf.Table.t
  ; weight : float Hf.Table.t
  }

let make sfb weight = { sfb; weight }

let incr ~(config : Config.t) : t Incr.t =
  let sfb =
    Sfb.v ~bigrams:config.bigrams
    |> Hf.Table.to_alist
    |> List.map ~f:(fun (key, data) -> map data ~f:(fun data -> key, data))
    |> all
    |> map ~f:Hf.Table.of_alist_exn
  in
  let weight =
    Weight.v ~monograms:config.monograms
    |> Hf.Table.to_alist
    |> List.map ~f:(fun (key, data) -> map data ~f:(fun data -> key, data))
    |> all
    |> map ~f:Hf.Table.of_alist_exn
  in
  map2 sfb weight ~f:(fun sfb weight -> { sfb; weight })
;;
