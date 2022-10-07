open! Import

type t =
  { sfb : float Hf.Map.t
  ; sfb_total : float
  ; dsfb : float Hf.Map.t
  ; dsfb_total : float
  ; speed : float Hf.Map.t
  ; speed_total : float
  ; lsb : float Hand.Map.t
  ; lsb_total : float
  ; keyfreq : float Hf.Map.t
  ; keyfreq_total : float
  ; hr_roll : float Hr.Map.t
  ; hr_roll_in : float Hr.Map.t
  ; hr_roll_out : float Hr.Map.t
  ; hr_roll_total : float
  ; hr_roll_in_total : float
  ; hr_roll_out_total : float
  ; hand_roll : float Hand.Map.t
  ; hand_roll_in : float Hand.Map.t
  ; hand_roll_out : float Hand.Map.t
  ; hand_roll_total : float
  ; hand_roll_in_total : float
  ; hand_roll_out_total : float
  ; dshrc : float Hand.Map.t
  ; dshrc_good : float Hand.Map.t
  ; dshrc_bad : float Hand.Map.t
  ; dshrc_total : float
  ; dshrc_good_total : float
  ; dshrc_bad_total : float
  }
[@@deriving sexp]

val to_string : t -> string
val incr : t Incr.t

(* same finger bigram *)

val sfb : float Hf.Map.t Incr.t
val sfb_total : float Incr.t

(* disjoint same finger bigram *)

val dsfb : float Hf.Map.t Incr.t
val dsfb_total : float Incr.t

(* finger speed *)

val speed : float Hf.Map.t Incr.t
val speed_total : float Incr.t

(* lateral stretch bigram *)

val lsb : float Hand.Map.t Incr.t
val lsb_total : float Incr.t

(* key frequency *)

val keyfreq : float Hf.Map.t Incr.t
val keyfreq_total : float Incr.t

(* rolls *)

val hr_roll : float Hr.Map.t Incr.t
val hr_roll_in : float Hr.Map.t Incr.t
val hr_roll_out : float Hr.Map.t Incr.t
val hr_roll_total : float Incr.t
val hr_roll_in_total : float Incr.t
val hr_roll_out_total : float Incr.t
val hand_roll : float Hand.Map.t Incr.t
val hand_roll_in : float Hand.Map.t Incr.t
val hand_roll_out : float Hand.Map.t Incr.t
val hand_roll_total : float Incr.t
val hand_roll_in_total : float Incr.t
val hand_roll_out_total : float Incr.t

(* unique finger *)

val dshrc : float Hand.Map.t Incr.t
val dshrc_good : float Hand.Map.t Incr.t
val dshrc_bad : float Hand.Map.t Incr.t
val dshrc_total : float Incr.t
val dshrc_good_total : float Incr.t
val dshrc_bad_total : float Incr.t
