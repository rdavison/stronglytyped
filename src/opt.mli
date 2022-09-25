open! Import

val neighbour : int -> unit
val anneal : kmax:int -> config:Config.t -> int * Time.Span.t
