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

let w_sfb = return 1.
let w_dsfb = return 2.

let w_keyfreq =
  return
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

let w_rolls = return 1.
let w_lsbs = return 1.
let neighbour = return (Neighbour.make (Neighbour.Config.make (`Random [ 1; 2; 3 ])))
let kmax = return 1_000_000
let progress_v = Var.create 0.
let progress = Var.watch progress_v
let set_progress i = Var.set progress_v i
