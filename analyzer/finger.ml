open! Import

module T = struct
  type t =
    [ `P
    | `R
    | `M
    | `I
    | `T
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
  | `T -> 4
;;

let of_int : int -> t = function
  | 0 -> `P
  | 1 -> `R
  | 2 -> `M
  | 3 -> `I
  | 4 -> `T
  | _ -> assert false
;;

let to_string = function
  | `P -> "P"
  | `R -> "R"
  | `M -> "M"
  | `I -> "I"
  | `T -> "T"
;;

let all : t list = [ `P; `R; `M; `I; `T ]

let of_char c =
  let c = Char.uppercase c in
  match c with
  | 'P' -> Some `P
  | 'R' -> Some `R
  | 'M' -> Some `M
  | 'I' -> Some `I
  | 'T' -> Some `T
  | _ -> None
;;

let of_string s =
  match String.lowercase s with
  | "p" | "pinky" -> Some `P
  | "r" | "ring" -> Some `R
  | "m" | "middle" -> Some `M
  | "i" | "index" -> Some `I
  | "t" | "thumb" -> Some `T
  | _ -> None
;;
