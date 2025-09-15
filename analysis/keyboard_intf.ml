open! Core

module type S = sig
  type t = Key.t Key.Id.Map.t [@@deriving sexp, bin_io, equal, compare]

  module Action : sig
    type t =
      | Swap of (Key.Id.t * Key.Id.t)
      | Random_swap
    [@@deriving sexp, compare, equal]
  end

  val state_machine : Bonsai.graph -> t Bonsai.t * (Action.t -> unit Ui_effect.t) Bonsai.t
end
