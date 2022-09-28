open! Import

module Config : sig
  (** Note: if using [`Curved a], then a must be a positive power of 2, otherwise [make] will raise *)
  type kind =
    [ `Const of int
    | `Linear
    | `Curved of int
    ]

  type t

  val make : ?max_swaps_at_once:int -> kind -> t
end

type t = float -> unit

val make : Config.t -> t
