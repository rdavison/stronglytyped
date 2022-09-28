open! Import

type t =
  { monograms : float Char.Table.t
  ; bigrams : float String.Table.t
  ; skipgrams : float String.Table.t
  ; neighbour : Neighbour.t
  ; kmax : int
  }

val w_sfb : float Incr.t
val w_dsfb : float Incr.t
val w_weight : float Finger.Table.t Incr.t
val w_rolls : float Incr.t
val monograms : float Char.Table.t Incr.t
val bigrams : float String.Table.t Incr.t
val skipgrams : float String.Table.t Incr.t
val neighbour : Neighbour.t Incr.t
val kmax : int Incr.t
val incr : t Incr.t
