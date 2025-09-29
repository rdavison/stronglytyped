open! Core
open! Bonsai
open! Bonsai.Let_syntax

module Mode = struct
  type t =
    | Manual
    | Optimize_browser
    | Optimize_server
  [@@deriving sexp, equal, compare, enumerate, bin_io]
end
