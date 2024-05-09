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
module C = Comparable.Make (T)
include C
module Infix : Comparable.Infix with type t := t = C
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

let gap t1 t2 =
  match t1, t2 with
  | `P, `R | `R, `P -> 1
  | `R, `M | `M, `R -> 1
  | `M, `I | `I, `M -> 1
  | `I, `T | `T, `I -> 1
  | `P, `M | `M, `P -> 2
  | `R, `I | `I, `R -> 2
  | `M, `T | `T, `M -> 2
  | `P, `I | `I, `P -> 3
  | `R, `T | `T, `R -> 3
  | `P, `T | `T, `P -> 4
  | `P, `P -> 0
  | `R, `R -> 0
  | `M, `M -> 0
  | `I, `I -> 0
  | `T, `T -> 0
;;

let adjacent f1 f2 = Int.equal (gap f1 f2) 1
