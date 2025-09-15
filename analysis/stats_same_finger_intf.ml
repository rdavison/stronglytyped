open! Core
open! Bonsai

module type S = sig
  type ('breakdown, 'total) metric =
    { breakdown : 'breakdown Hand_finger.Map.t
    ; total : 'total
    }
  [@@deriving sexp, compare, equal]

  type t =
    | Sfb of (float, float) metric
    | Sfs of (float, float) metric
    | Speed of (float, float) metric
    | Sfb_worst of ((string * float) list, float) metric
    | Sfs_worst of ((string * float) list, float) metric
    | Speed_worst of ((string * float) list, float) metric
  [@@deriving sexp, compare, equal, typed_variants]

  val bigram : Typed_variant.Packed.t -> Bigram_data.info -> float

  val component
    :  metrics:
         (Typed_variant.Packed.t, Typed_variant.Packed.comparator_witness) Base.Set.t
           Bonsai.t
    -> worst_counter:int Bonsai.t
    -> diff_row_bigram_data:Bigram_data.t Hand_finger.Map.t Bonsai.t
    -> Bonsai.graph
    -> (Typed_variant.Packed.t, t, Typed_variant.Packed.comparator_witness) Core.Map.t
         Bonsai.t

  val bigram_data
    :  Bigram_data.info Key.Id.Pair.Map.t Bonsai.t
    -> Bonsai.graph
    -> Bigram_data.t Hand_finger.Map.t Bonsai.t
end
