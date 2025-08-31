open! Core

type t =
  [ `Alpha of char
  | `Sym of char * char
  | `Legend of string
  | `Power
  ]
[@@deriving sexp, equal, compare]

let to_string_upper t =
  match t with
  | `Alpha c -> sprintf "%c" (Char.uppercase c)
  | `Sym (_lower, upper) -> sprintf "%c" upper
  | `Legend _ | `Power -> failwith "keycode not supported"
;;

let to_string_lower t =
  match t with
  | `Alpha c -> sprintf "%c" (Char.lowercase c)
  | `Sym (lower, _upper) -> sprintf "%c" lower
  | `Legend _ | `Power -> failwith "keycode not supported"
;;
