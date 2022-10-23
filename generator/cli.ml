open! Import

let param =
  let%map_open.Command () = return () in
  fun () ->
    let analysis = Analyzer.Analysis.make_incr Stats.sfb_total in
    let keyboard = Keyboard.make analysis in
    Cjalgorithm.start 6 ~keyboard
;;
