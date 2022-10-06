open! Import

(** same finger bigram *)
val sfb : float Hf.Map.t Incr.t

val sfb_total : float Incr.t

(** disjoint same finger bigram *)
val dsfb : float Hf.Map.t Incr.t

val dsfb_total : float Incr.t

(** finger speed *)
val speed : float Hf.Map.t Incr.t

val speed_total : float Incr.t

(** lateral stretch bigram *)
val lsb : float Hand.Map.t Incr.t

val lsb_total : float Incr.t

(** key frequency *)
val keyfreq : float Hf.Map.t Incr.t

val keyfreq_total : float Incr.t

(** rolls *)
val roll : float Hr.Map.t Incr.t

val roll_total : float Incr.t

(** unique finger *)
val uf : (float * float) Hand.Map.t Incr.t

val uf_total : (float * float) Incr.t
