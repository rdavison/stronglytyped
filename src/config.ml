open! Import

module Vars = struct
  module C = struct
    let sf c =
      let dexterity v =
        match v with
        | `P -> 0.1
        | `R -> 0.2
        | `M -> 0.3
        | `I -> 0.4
      in
      Finger.all
      |> List.map ~f:(fun v -> v, (c, (1. -. dexterity v) *. 20.))
      |> Finger.Map.of_alist_exn
      |> Incr.Var.create
    ;;

    let sfb = sf 0.
    let dsfb = sf 0.
    let speed = sf 0.
    let roll = Incr.Var.create (0., 1., 1.)
    let lsb = Incr.Var.create (0., 1000.)
    let brc = Incr.Var.create (0., 1.)
    let shb = Incr.Var.create 0.2
    let shs = Incr.Var.create 0.2

    let keyfreq =
      Incr.Var.create
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

  let progress = Incr.Var.create 0.

  let neighbor_v =
    Incr.Var.create (Neighbor.make (Neighbor.Config.make (`Random [ 1; 2; 3; 4 ])))
  ;;

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
    let brc = Incr.Var.watch Vars.C.brc
    let speed = Incr.Var.watch Vars.C.speed
  end

  let neighbor = Incr.Var.watch Vars.neighbor_v
  let kmax = Incr.Var.watch Vars.kmax_v
  let progress = Incr.Var.watch Vars.progress
end
