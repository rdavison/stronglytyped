open! Import

module T = struct
  type t =
    [ `L
    | `R
    ]
  [@@deriving sexp, compare, hash, equal]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let to_int : t -> int = function
  | `L -> 0
  | `R -> 1
;;

let of_int : int -> t = function
  | 0 -> `L
  | 1 -> `R
  | _ -> assert false
;;

let all : t list = [ `L; `R ]