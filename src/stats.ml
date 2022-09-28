open! Import
open! Incr

type t =
  { sfb : float Hf.Table.t
  ; dsfb : float Hf.Table.t
  ; weight : float Hf.Table.t
  }

let make sfb dsfb weight = { sfb; dsfb; weight }

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
  let dsfb =
    Dsfb.table ~skipgrams:config.bigrams
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
  map3 sfb dsfb weight ~f:(fun sfb dsfb weight -> { sfb; dsfb; weight })
;;
