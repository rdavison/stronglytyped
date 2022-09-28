open! Import

type t =
  { monograms : float Char.Table.t
  ; bigrams : float String.Table.t
  ; neighbour : Neighbour.t
  ; kmax : int
  }

val monograms : float Char.Table.t Incr.t
val bigrams : float String.Table.t Incr.t
val neighbour : Neighbour.t Incr.t
val kmax : int Incr.t
val incr : t Incr.t
