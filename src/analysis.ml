open! Import
open! Incr

type t =
  { score : Score.t
  ; pretty : Pretty.t
  }

let v ~config =
  let score = Score.incr ~config in
  map2 score Pretty.v ~f:(fun score pretty -> { score; pretty })
;;