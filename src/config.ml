open! Import
open! Incr

type t =
  { monograms : float Char.Table.t
  ; bigrams : float String.Table.t
  ; neighbour : Neighbour.t
  ; kmax : int
  }

let monograms = return (Char.Table.create ())
let bigrams = return (String.Table.create ())
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
