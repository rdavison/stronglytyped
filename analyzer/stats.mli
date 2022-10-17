open! Import

type 'a t =
  { sfb : 'a Hf.Map.t
  ; sfb_total : 'a
  ; dsfb : 'a Hf.Map.t
  ; dsfb_total : 'a
  ; speed : 'a Hf.Map.t
  ; speed_total : 'a
  ; lsb : 'a Hand.Map.t
  ; lsb_total : 'a
  ; keyfreq : 'a Hf.Map.t
  ; keyfreq_total : 'a
  ; roll : 'a Hand.Map.t
  ; roll_total : 'a
  ; roll_top : 'a Hand.Map.t
  ; roll_top_total : 'a
  ; roll_middle : 'a Hand.Map.t
  ; roll_middle_total : 'a
  ; roll_bottom : 'a Hand.Map.t
  ; roll_bottom_total : 'a
  ; roll_in : 'a Hand.Map.t
  ; roll_in_total : 'a
  ; roll_in_top : 'a Hand.Map.t
  ; roll_in_top_total : 'a
  ; roll_in_middle : 'a Hand.Map.t
  ; roll_in_middle_total : 'a
  ; roll_in_bottom : 'a Hand.Map.t
  ; roll_in_bottom_total : 'a
  ; roll_out : 'a Hand.Map.t
  ; roll_out_total : 'a
  ; roll_out_top : 'a Hand.Map.t
  ; roll_out_top_total : 'a
  ; roll_out_middle : 'a Hand.Map.t
  ; roll_out_middle_total : 'a
  ; roll_out_bottom : 'a Hand.Map.t
  ; roll_out_bottom_total : 'a
  ; dshrc : 'a Hand.Map.t
  ; dshrc_total : 'a
  ; dshrc_good : 'a Hand.Map.t
  ; dshrc_good_total : 'a
  ; dshrc_bad : 'a Hand.Map.t
  ; dshrc_bad_total : 'a
  }
[@@deriving sexp, compare]

val worst : float t
val both : 'a t -> 'b t -> ('a * 'b) t

module Internal : sig
  module Sfb : sig
    val keyset : Hf.t -> (Key.t * Key.t) list Incr.t
  end

  module Dsfb : sig
    val keyset : Hf.t -> (Key.t * Key.t) list Incr.t
  end

  module Speed : sig
    val keyset : Hf.t -> (Key.t * Key.t) list Incr.t
  end

  module Lsb : sig
    val keyset : Hand.t -> (Key.t * Key.t) list Incr.t
  end

  module Roll : sig
    val keyset : Hand.t * int -> (Key.t * Key.t) list Incr.t
  end

  module Dshrc : sig
    val keyset : Hand.t -> (Key.t * Key.t) list Incr.t
  end
end

val to_string : float t -> string
val incr : float t Incr.t

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
