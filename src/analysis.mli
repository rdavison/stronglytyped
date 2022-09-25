open! Import

type t =
  { score : Score.t
  ; pretty : Pretty.t
  }

val v : config:Config.t -> t Incr.t