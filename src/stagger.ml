open! Import

type t =
  [ `Matrix
  | `Colstag
  | `Typewriter
  | `HHKB
  ]

let default = `HHKB

let to_string t =
  match t with
  | `Matrix -> "Matrix"
  | `Colstag -> "Colstag"
  | `Typewriter -> "Typewriter"
  | `HHKB -> "HHKB"
;;

let row_offset t row =
  match t, row with
  | `Matrix, _ -> 0.
  | `Typewriter, 0 -> -0.25
  | `Typewriter, 1 -> 0.
  | `Typewriter, 2 -> 0.50
  | `HHKB, 0 -> -0.25
  | `HHKB, 1 -> 0.
  | `HHKB, 2 -> 0.25
  | _, _ -> failwithf "Undefined: (%s, %d)" (to_string t) row ()
;;

let var = Incr.Var.create default
let incr = Incr.Var.watch var