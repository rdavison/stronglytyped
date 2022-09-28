open! Import

module T = struct
  type t = Hand.t * int [@@deriving sexp, compare, hash, equal]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let all =
  let open List.Let_syntax in
  let%map_open h = Hand.all
  and r = [ 0; 1; 2 ] in
  h, r
;;
