open! Import
open! Incr

module Vars = struct
  module C = struct
    let sfb = Incr.Var.create 0.
    let dsfb = Incr.Var.create 0.05
    let roll = Incr.Var.create 0.04
    let lsb = Incr.Var.create 0.
    let speed = Incr.Var.create 0.
    let shb = Incr.Var.create 0.2
    let shs = Incr.Var.create 0.2

    let keyfreq =
      Var.create
        (Finger.all
        |> List.map ~f:(fun finger ->
               ( finger
               , 1.
                 -.
                 match finger with
                 | `P -> 1.5 /. 5.5
                 | `R -> 3.6 /. 5.5
                 | `M -> 4.8 /. 5.5
                 | `I -> 5.5 /. 5.5 ))
        |> Finger.Table.of_alist_exn)
    ;;
  end

  let progress = Var.create 0.
  let neighbour_v = Incr.Var.create (Neighbour.make (Neighbour.Config.make (`Curved 4)))
  let kmax_v = Incr.Var.create 1_000_000
end

module Incr = struct
  module C = struct
    let sfb = Incr.Var.watch Vars.C.sfb
    let dsfb = Incr.Var.watch Vars.C.dsfb
    let shb = Incr.Var.watch Vars.C.shb
    let shs = Incr.Var.watch Vars.C.shs
    let keyfreq = Incr.Var.watch Vars.C.keyfreq
    let roll = Incr.Var.watch Vars.C.roll
    let lsb = Incr.Var.watch Vars.C.lsb
    let speed = Incr.Var.watch Vars.C.speed
  end

  let neighbour = Incr.Var.watch Vars.neighbour_v
  let kmax = Incr.Var.watch Vars.kmax_v
  let progress = Var.watch Vars.progress
end
