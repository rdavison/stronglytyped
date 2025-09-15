open! Core
open! Bonsai
open! Bonsai.Let_syntax

module Mode = struct
  type t =
    | Auto
    | Manual
  [@@deriving sexp, equal, compare]

  let start runtime_mode ~f graph =
    let bonsai_unit =
      match%sub runtime_mode with
      | Manual -> Bonsai.return ()
      | Auto ->
        Bonsai.Edge.lifecycle ~after_display:f graph;
        Bonsai.return ()
    in
    (ignore : unit Bonsai.t -> unit) bonsai_unit
  ;;
end
