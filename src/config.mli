open! Import

val w_sfb : float Incr.t
val w_dsfb : float Incr.t
val w_keyfreq : float Finger.Table.t Incr.t
val w_rolls : float Incr.t
val w_lsbs : float Incr.t
val monograms : float Char.Table.t Incr.t
val bigrams : float String.Table.t Incr.t
val skipgrams : float String.Table.t Incr.t
val neighbour : Neighbour.t Incr.t
val kmax : int Incr.t
val progress : float Incr.t
val set_progress : float -> unit
