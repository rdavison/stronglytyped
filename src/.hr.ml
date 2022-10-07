open! Import

module T = struct
  type t = Hand.t * int [@@deriving sexp, compare, hash, equal]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let all =
  let%map_open.List h = Hand.all
  and r = [ 0; 1; 2 ] in
  h, r
;;

let to_string (h, r) = sprintf "%s%d" (Hand.to_string h) r
