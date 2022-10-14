open! Import

module T = struct
  type t = Hand.t * Finger.t [@@deriving sexp, compare, hash, equal]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let to_int (h, f) =
  match h, f with
  | `L, `P -> 0
  | `L, `R -> 1
  | `L, `M -> 2
  | `L, `I -> 3
  | `R, `I -> 4
  | `R, `M -> 5
  | `R, `R -> 6
  | `R, `P -> 7
;;

let of_int = function
  | 0 -> `L, `P
  | 1 -> `L, `R
  | 2 -> `L, `M
  | 3 -> `L, `I
  | 4 -> `R, `I
  | 5 -> `R, `M
  | 6 -> `R, `R
  | 7 -> `R, `P
  | _ -> assert false
;;

let to_string (h, f) = sprintf "%s%s" (Hand.to_string h) (Finger.to_string f)
let all = [ `L, `P; `L, `R; `L, `M; `L, `I; `R, `I; `R, `M; `R, `R; `R, `P ]
