open! Import

type t =
  | Stop
  | Reset
  | Ready
  | Run of Run.t

let var = Incr.Var.create Reset
let incr = Incr.Var.watch var
let observer = Incr.observe incr