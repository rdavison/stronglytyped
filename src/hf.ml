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

let all = List.concat_map Hand.all ~f:(fun h -> List.map Finger.all ~f:(fun f -> h, f))
