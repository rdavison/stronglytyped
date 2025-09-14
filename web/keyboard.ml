open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Keycode = Analysis.Keycode
module Keyboard = Analysis.Keyboard

let vdom keyboard corpus_freq_a max_value =
  let arrangement = Analysis.Arrangement.ansi in
  let row row =
    row
    |> List.map ~f:(fun id ->
      let%arr keyboard = keyboard
      and corpus_freq_a = corpus_freq_a
      and max_value = max_value in
      let k = Map.find keyboard id in
      Key.vdom id k corpus_freq_a max_value)
    |> Bonsai.all
    |> Bonsai.map ~f:(Vdom.Node.div ~attrs:[ Style.keyboard_row ])
  in
  let vdom arrangement =
    arrangement
    |> List.map ~f:row
    |> Bonsai.all
    |> Bonsai.map ~f:(fun keyboard_rows ->
      Vdom.Node.div ~attrs:[ Style.keyboard; Style.Variables.set () ] keyboard_rows)
  in
  vdom arrangement
;;

let component keyboard (corpus : Analysis.Corpus.t Bonsai.t) graph =
  let corpus_freq_a = Bonsai.map corpus ~f:(fun corpus -> corpus.freq.a) in
  let max_value =
    Bonsai.Map.max_value corpus_freq_a ~comparator:(module Float) graph
    |> Bonsai.map ~f:(Option.value ~default:1.)
  in
  let%arr vdom = vdom keyboard corpus_freq_a max_value in
  Vdom.Node.div ~attrs:[ Style.keyboard_section ] [ vdom ]
;;
