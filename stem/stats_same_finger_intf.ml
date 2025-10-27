open! Import

module type S = sig
  type ('breakdown, 'total) metric =
    { breakdown : 'breakdown
    ; total : 'total
    }
  [@@deriving sexp, compare, equal, bin_io]

  type t =
    | Sfb of (float Hand_finger.Map.t, float) metric
    | Sfs of (float Hand_finger.Map.t, float) metric
    | Speed of (float Hand_finger.Map.t, float) metric
    | Sfb_worst of (((string * float) list * float) Hand_finger.Map.t, float) metric
    | Sfs_worst of (((string * float) list * float) Hand_finger.Map.t, float) metric
    | Speed_worst of (((string * float) list * float) Hand_finger.Map.t, float) metric
  [@@deriving sexp, compare, equal, typed_variants, bin_io]

  val bigram : Typed_variant.Packed.t -> Bigram_data.info -> float

  val component
    :  metrics:
         (Typed_variant.Packed.t, Typed_variant.Packed.comparator_witness) Base.Set.t
           Bonsai.t
    -> worst_counter:int Bonsai.t
    -> diff_row_bigram_data:(Keyboard.t * Bigram_data.t Hand_finger.Map.t) Bonsai.t
    -> Bonsai.graph
    -> (Keyboard.t
       * (Typed_variant.Packed.t, t, Typed_variant.Packed.comparator_witness) Core.Map.t)
         Bonsai.t

  val bigram_data
    :  (Keyboard.t * Bigram_data.info Key.Id.Pair.Map.t) Bonsai.t
    -> Bonsai.graph
    -> (Keyboard.t * Bigram_data.t Hand_finger.Map.t) Bonsai.t
end
