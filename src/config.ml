open! Import

module Var = struct
  module C = struct
    let sf c =
      let dexterity v =
        match v with
        | `P -> 0.2
        | `R -> 0.3
        | `M -> 0.5
        | `I -> 0.8
      in
      Finger.all
      |> List.map ~f:(fun v -> v, (c, 1. -. dexterity v))
      |> Finger.Map.of_alist_exn
      |> Incr.Var.create
    ;;

    let sfb = sf 0.
    let dsfb = sf 0.
    let speed = sf 0.
    let roll = Incr.Var.create (0., 1., 1.)
    let lsb = Incr.Var.create (0., 1000.)
    let uf = Incr.Var.create (0., 1.)

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
  let gen = Incr.Var.create false
end

module Incr = struct
  module C = struct
    let sfb = Incr.Var.watch Var.C.sfb
    let dsfb = Incr.Var.watch Var.C.dsfb
    let keyfreq = Incr.Var.watch Var.C.keyfreq
    let roll = Incr.Var.watch Var.C.roll
    let lsb = Incr.Var.watch Var.C.lsb
    let uf = Incr.Var.watch Var.C.uf
    let speed = Incr.Var.watch Var.C.speed
  end

  let neighbor = Incr.Var.watch Var.neighbor_v
  let kmax = Incr.Var.watch Var.kmax_v
  let progress = Incr.Var.watch Var.progress
  let gen = Incr.Var.watch Var.gen
end
