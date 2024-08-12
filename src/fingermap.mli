open! Import

type t =
  | Standard
  | Angle
  | Enigmak
  | Custom of Finger.t array
[@@deriving sexp]

val to_string : t -> string
val of_string : string -> t

module Command : sig
  val default : t
  val possible_values : string list
  val arg_type : t Command.Param.Arg_type.t
end
