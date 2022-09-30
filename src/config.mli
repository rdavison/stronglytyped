open! Import

val c_sfb_v : float Incr.Var.t
val c_dsfb_v : float Incr.Var.t
val c_keyfreq_v : float Finger.Table.t Incr.Var.t
val w_roll_v : float Incr.Var.t
val w_lsb_v : float Incr.Var.t
val neighbour_v : Neighbour.t Incr.Var.t
val kmax_v : int Incr.Var.t
val progress_v : float Incr.Var.t
val c_sfb : float Incr.t
val c_dsfb : float Incr.t
val c_keyfreq : float Finger.Table.t Incr.t
val w_roll : float Incr.t
val w_lsb : float Incr.t
val monograms : float Char.Table.t Incr.t
val bigrams : float String.Table.t Incr.t
val skipgrams : float String.Table.t Incr.t
val neighbour : Neighbour.t Incr.t
val kmax : int Incr.t
val progress : float Incr.t
