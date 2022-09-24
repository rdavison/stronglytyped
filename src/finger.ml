open! Import

module T = struct
  type t =
    [ `P
    | `R
    | `M
    | `I
    ]
  [@@deriving sexp, compare, hash, equal]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let to_int : t -> int = function
  | `P -> 0
  | `R -> 1
  | `M -> 2
  | `I -> 3
;;

let of_int : int -> t = function
  | 0 -> `P
  | 1 -> `R
  | 2 -> `M
  | 3 -> `I
  | _ -> assert false
;;

let all : t list = [ `P; `R; `M; `I ]
