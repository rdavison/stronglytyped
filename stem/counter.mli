open! Import

module Action : sig
  type t =
    | Increment
    | Decrement
  [@@deriving sexp]
end

val counter
  :  int
  -> Bonsai.graph
  -> int Bonsai.t * (Action.t -> unit Bonsai.Effect.t) Bonsai.t
