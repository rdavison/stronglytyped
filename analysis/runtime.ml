open! Core
open! Bonsai
open! Bonsai.Let_syntax

module Mode = struct
  type t =
    | Auto
    | Manual
    | Optimize
  [@@deriving sexp, equal, compare, enumerate, bin_io]
end
