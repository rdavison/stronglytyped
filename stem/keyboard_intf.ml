open! Import

module type S = sig
  type t = Key.t Key.Id.Map.t [@@deriving sexp, bin_io, equal, compare]

  module Action : sig
    type t =
      | Swap of (Key.Id.t * Key.Id.t)
      | Random_swap
      | Overwrite of Keycode.t Key.Id.Map.t
    [@@deriving sexp, compare, equal]
  end

  val state_machine
    :  Bonsai.graph
    -> t Bonsai.t
       * (Action.t list -> unit Ui_effect.t) Bonsai.t
       * unit Ui_effect.t Bonsai.t

  val swap : t -> Key.Id.Pair.t -> t option
  val all_swaps : t Bonsai.t -> Bonsai.graph -> t Key.Id.Pair.Map.t Bonsai.t
  val to_string : t -> string
end
