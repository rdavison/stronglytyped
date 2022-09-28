open! Import
open! Incr

type t = float

let make keys ~skipgrams = Sfb.make keys ~bigrams:skipgrams
let table ~skipgrams = By_hf.table |> Hf.Table.map ~f:(map ~f:(make ~skipgrams))
