open! Core

type t = Key.t Key.Id.Map.t [@@deriving sexp, equal, compare]
