open! Import

module T = struct
  type t = Hand.t * Finger.t [@@deriving sexp, compare, hash, equal]
end

include T
module C = Comparable.Make (T)
include C
module Infix : Comparable.Infix with type t := t = C
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
  | `L, `T -> 8
  | `R, `T -> 9
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
  | 8 -> `L, `T
  | 9 -> `R, `T
  | _ -> assert false
;;

let adjacent_no_thumb t1 t2 =
  match t1, t2 with
  | (`L, _), (`R, _) | (`R, _), (`L, _) -> false
  | (_, `P), (_, `R)
  | (_, `R), (_, `P)
  | (_, `R), (_, `M)
  | (_, `M), (_, `R)
  | (_, `M), (_, `I)
  | (_, `I), (_, `M) -> true
  | (_, _), (_, _) -> false
;;

let to_string (h, f) = sprintf "%s%s" (Hand.to_string h) (Finger.to_string f)
let all = List.cartesian_product Hand.all Finger.all

let of_string s =
  match Int.equal (String.length s) 2 with
  | false -> None
  | true ->
    let h = Hand.of_char s.[0] in
    let f = Finger.of_char s.[1] in
    Option.both h f
;;
