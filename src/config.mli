open! Import

type t =
  { monograms : float Char.Table.t
  ; bigrams : float String.Table.t
  }

val create : unit -> t
