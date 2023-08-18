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
  | `L -> 1
  | `R -> 2
;;

let of_int : int -> t = function
  | 1 -> `L
  | 2 -> `R
  | _ -> assert false
;;

let to_string = function
  | `L -> "L"
  | `R -> "R"
;;

let all : t list = [ `L; `R ]

let of_char c =
  let c = Char.uppercase c in
  match c with
  | 'L' -> Some `L
  | 'R' -> Some `R
  | _ -> None
;;

let of_string s =
  match String.lowercase s with
  | "left" | "l" -> Some `L
  | "right" | "r" -> Some `R
  | _ -> None
;;
