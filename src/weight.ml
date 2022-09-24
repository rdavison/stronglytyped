open! Import
open! Incr

type t = float Incr.t Hf.Table.t

let make keys ~monograms =
  List.fold keys ~init:0. ~f:(fun acc key ->
      acc
      +.
      match key.Key.code with
      | `Char code -> Char.Table.find_or_add monograms code ~default:(fun () -> 0.))
;;

let v ~monograms : t = By_hf.v |> Hf.Table.map ~f:(map ~f:(make ~monograms))
