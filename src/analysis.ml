open! Import
open! Incr

type t =
  { score : Score.t
  ; pretty : Pretty.t
  }

let incr = map2 Score.incr Pretty.incr ~f:(fun score pretty -> { score; pretty })
