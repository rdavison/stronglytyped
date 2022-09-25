open! Import

type t =
  { sfb : float Hf.Table.t
  ; weight : float Hf.Table.t
  }

val make : float Hf.Table.t -> float Hf.Table.t -> t
val incr : config:Config.t -> t Incr.t