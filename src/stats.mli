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
  ; roll : float Hand.Map.t
  ; roll_total : float
  ; roll_top : float Hand.Map.t
  ; roll_top_total : float
  ; roll_middle : float Hand.Map.t
  ; roll_middle_total : float
  ; roll_bottom : float Hand.Map.t
  ; roll_bottom_total : float
  ; roll_in : float Hand.Map.t
  ; roll_in_total : float
  ; roll_in_top : float Hand.Map.t
  ; roll_in_top_total : float
  ; roll_in_middle : float Hand.Map.t
  ; roll_in_middle_total : float
  ; roll_in_bottom : float Hand.Map.t
  ; roll_in_bottom_total : float
  ; roll_out : float Hand.Map.t
  ; roll_out_total : float
  ; roll_out_top : float Hand.Map.t
  ; roll_out_top_total : float
  ; roll_out_middle : float Hand.Map.t
  ; roll_out_middle_total : float
  ; roll_out_bottom : float Hand.Map.t
  ; roll_out_bottom_total : float
  ; dshrc : float Hand.Map.t
  ; dshrc_total : float
  ; dshrc_good : float Hand.Map.t
  ; dshrc_good_total : float
  ; dshrc_bad : float Hand.Map.t
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

val roll : float Hand.Map.t Incr.t
val roll_total : float Incr.t
val roll_top : float Hand.Map.t Incr.t
val roll_top_total : float Incr.t
val roll_middle : float Hand.Map.t Incr.t
val roll_middle_total : float Incr.t
val roll_bottom : float Hand.Map.t Incr.t
val roll_bottom_total : float Incr.t
val roll_in : float Hand.Map.t Incr.t
val roll_in_total : float Incr.t
val roll_in_top : float Hand.Map.t Incr.t
val roll_in_top_total : float Incr.t
val roll_in_middle : float Hand.Map.t Incr.t
val roll_in_middle_total : float Incr.t
val roll_in_bottom : float Hand.Map.t Incr.t
val roll_in_bottom_total : float Incr.t
val roll_out : float Hand.Map.t Incr.t
val roll_out_total : float Incr.t
val roll_out_top : float Hand.Map.t Incr.t
val roll_out_top_total : float Incr.t
val roll_out_middle : float Hand.Map.t Incr.t
val roll_out_middle_total : float Incr.t
val roll_out_bottom : float Hand.Map.t Incr.t
val roll_out_bottom_total : float Incr.t

(* unique finger *)

val dshrc : float Hand.Map.t Incr.t
val dshrc_total : float Incr.t
val dshrc_good : float Hand.Map.t Incr.t
val dshrc_good_total : float Incr.t
val dshrc_bad : float Hand.Map.t Incr.t
val dshrc_bad_total : float Incr.t
