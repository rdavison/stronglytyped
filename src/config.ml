open! Import
open! Incr

type t =
  { monograms : float Char.Table.t
  ; bigrams : float String.Table.t
  ; neighbour : Neighbour.t
  ; kmax : int
  }

let n v =
  let total = Hashtbl.data v |> List.sum (module Float) ~f:Fn.id in
  Hashtbl.map v ~f:(fun x -> x /. total)
;;

let monograms = map Corpus.incr ~f:(fun v -> n v.Corpus.singles)
let bigrams = map Corpus.incr ~f:(fun v -> n v.Corpus.s1)
let neighbour = return (Neighbour.make (Neighbour.Config.make (`Const 1)))
let kmax = return 1_000_000

let incr =
  let open Let_syntax in
  let%map_open monograms = monograms
  and bigrams = bigrams
  and neighbour = neighbour
  and kmax = kmax in
  { monograms; bigrams; neighbour; kmax }
;;
