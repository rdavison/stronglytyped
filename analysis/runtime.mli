open! Core
open! Bonsai

module Mode : sig
  type t =
    | Auto
    | Manual
    | Optimize
  [@@deriving sexp, compare, equal, enumerate, bin_io]
end
