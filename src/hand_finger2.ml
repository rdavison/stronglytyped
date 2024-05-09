open! Import

module T = struct
  type t = Hand.t * Finger.t * Finger.t [@@deriving sexp, compare, hash, equal]
end

include T
module C = Comparable.Make (T)
include C
module Infix : Comparable.Infix with type t := t = C
include Hashable.Make (T)

let to_string (h, f1, f2) =
  sprintf "%s%s%s" (Hand.to_string h) (Finger.to_string f1) (Finger.to_string f2)
;;

let all : t list =
  let open List.Let_syntax in
  let%bind h = Hand.all in
  let%bind f1 = Finger.all in
  let%map f2 = Finger.all in
  h, f1, f2
;;

let of_string s =
  match Int.equal (String.length s) 2 with
  | false -> None
  | true ->
    let open Option.Let_syntax in
    let%map_open h = Hand.of_char s.[0]
    and f1 = Finger.of_char s.[1]
    and f2 = Finger.of_char s.[2] in
    h, f1, f2
;;
