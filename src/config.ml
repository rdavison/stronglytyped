open! Import
open! Incr

let n v =
  let total = Hashtbl.data v |> List.sum (module Float) ~f:Fn.id in
  Hashtbl.map v ~f:(fun x -> x /. total)
;;

let monograms = map Corpus.incr ~f:(fun v -> n v.Corpus.singles)
let bigrams = map Corpus.incr ~f:(fun v -> n v.Corpus.s1)

let skipgrams =
  map Corpus.incr ~f:(fun v ->
      let acc = String.Table.create () in
      [ v.Corpus.s2; v.s3; v.s4; v.s5; v.s6; v.s7; v.s8 ]
      |> List.fold ~init:2 ~f:(fun denom s ->
             String.Table.merge_into ~src:s ~dst:acc ~f:(fun ~key:_ a maybe_b ->
                 let res =
                   (a /. Float.of_int denom) +. Option.value maybe_b ~default:0.
                 in
                 Hashtbl.Merge_into_action.Set_to res);
             denom + 1)
      |> ignore;
      acc)
;;

let c_sfb_v = Incr.Var.create 1.
let c_sfb = Incr.Var.watch c_sfb_v
let c_dsfb_v = Incr.Var.create 2.
let c_dsfb = Incr.Var.watch c_dsfb_v

let c_keyfreq_v =
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

let c_keyfreq = Incr.Var.watch c_keyfreq_v
let w_roll_v = Incr.Var.create 1.
let w_roll = Incr.Var.watch w_roll_v
let w_lsb_v = Incr.Var.create 1.
let w_lsb = Incr.Var.watch w_lsb_v

let neighbour_v =
  Incr.Var.create (Neighbour.make (Neighbour.Config.make (`Random [ 1; 2; 3 ])))
;;

let neighbour = Incr.Var.watch neighbour_v
let kmax_v = Incr.Var.create 1_000_000
let kmax = Incr.Var.watch kmax_v
let progress_v = Var.create 0.
let progress = Var.watch progress_v
