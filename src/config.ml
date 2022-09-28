open! Import
open! Incr

type t =
  { monograms : float Char.Table.t
  ; bigrams : float String.Table.t
  ; skipgrams : float String.Table.t
  ; neighbour : Neighbour.t
  ; kmax : int
  }

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

let neighbour = return (Neighbour.make (Neighbour.Config.make (`Random [ 1; 2; 3 ])))
let kmax = return 1_000_000

let incr =
  let open Let_syntax in
  let%map_open monograms = monograms
  and bigrams = bigrams
  and skipgrams = skipgrams
  and neighbour = neighbour
  and kmax = kmax in
  { monograms; bigrams; skipgrams; neighbour; kmax }
;;
