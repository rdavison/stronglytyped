open! Import

type t =
  { monograms : float Char.Table.t
  ; bigrams : float String.Table.t
  }

let create () = { monograms = Char.Table.create (); bigrams = String.Table.create () }
