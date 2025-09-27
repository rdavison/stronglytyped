open! Core
open! Bonsai
open! Bonsai.Let_syntax
module M = Computation_status

module Action = struct
  type t = |
end

let gen graph =
  let corpus = Bonsai.return Corpus.fast in
  let keyboard, keyboard_inject, _cancel = Keyboard.state_machine graph in
  let random_swap_effect =
    let%arr keyboard_inject = keyboard_inject in
    keyboard_inject [ Random_swap ]
  in
  Bonsai.Edge.after_display random_swap_effect graph;
  let metrics =
    Base.Set.of_list
      (module Stats_same_finger.Typed_variant.Packed)
      Stats_same_finger.Typed_variant.Packed.all
    |> Bonsai.return
  in
  let worst_counter =
    let n, _inject = Counter.counter 6 graph in
    n
  in
  let diff_row_bigram_data =
    let data = Bigram_data.make keyboard corpus graph in
    Stats_same_finger.bigram_data data graph
  in
  let same_finger_stats =
    Stats_same_finger.component ~metrics ~worst_counter ~diff_row_bigram_data graph
  in
  let score = Score.score ~same_finger_stats in
  let window, window_insert =
    Window.descending ~size:(Bonsai.return 10) ~compare:Float.compare graph
  in
  let score =
    let%map score = score in
    Option.value score ~default:Float.max_value
  in
  Bonsai.Edge.on_change
    ~equal:[%equal: float * Keyboard.t]
    (Bonsai.both score keyboard)
    ~callback:window_insert
    graph;
  let window =
    let%map window = window in
    List.rev window
  in
  keyboard, window
;;
