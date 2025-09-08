open! Core

type t = Key.t Key.Id.Map.t [@@deriving sexp, bin_io, equal, compare]
