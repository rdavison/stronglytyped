open! Core
open! Bonsai

module Hand_finger : sig
  type t =
    { freqs : float Hand_finger.Map.t
    ; total : float
    ; worst_n : (string * float) list Hand_finger.Map.t
    ; worst_n_total : float
    }

  val diff_row_bigram_data
    :  bigram_data:Bigram_data.info Key.Id.Pair.Map.t Bonsai.t
    -> Bonsai.graph
    -> Bigram_data.t Hand_finger.Map.t Bonsai.t

  val sfb
    :  diff_row_bigram_data:Bigram_data.t Hand_finger.Map.t Bonsai.t
    -> worst_counter:int Bonsai.t
    -> Bonsai.graph
    -> t Bonsai.t

  val sfs
    :  diff_row_bigram_data:Bigram_data.t Hand_finger.Map.t Bonsai.t
    -> worst_counter:int Bonsai.t
    -> Bonsai.graph
    -> t Bonsai.t

  val speed
    :  diff_row_bigram_data:Bigram_data.t Hand_finger.Map.t Bonsai.t
    -> worst_counter:int Bonsai.t
    -> Bonsai.graph
    -> t Bonsai.t
end
