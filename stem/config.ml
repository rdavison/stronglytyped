open! Import

type t =
  { corpus : Corpus.t
  ; finger_dexterity : float
  }
[@@deriving sexp, equal, compare, bin_io]
