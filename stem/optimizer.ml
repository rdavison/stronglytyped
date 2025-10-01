open! Import

type t =
  | Random
  | Greedy
[@@deriving sexp, compare, equal, enumerate, bin_io]

let default = Random

let score ~keyboard ~metrics ~worst_counter ~diff_row_bigram_data graph =
  let same_finger_stats =
    Stats_same_finger.component ~metrics ~worst_counter ~diff_row_bigram_data graph
  in
  Score.score ~same_finger_stats
;;

module Scored_keyboard = struct
  module T = struct
    type t = float * Keyboard.t [@@deriving sexp, compare, equal]
  end

  include T
  include Comparator.Make (T)
end

let random
      ~corpus
      ~keyboard
      ~keyboard_inject
      ~window_insert
      ~metrics
      ~worst_counter
      ~diff_row_bigram_data
      ~score
      graph
  =
  Bonsai.Edge.on_change
    ~equal:Scored_keyboard.equal
    (Bonsai.both (score keyboard) keyboard)
    ~callback:
      (let%arr window_insert = window_insert
       and keyboard_inject = keyboard_inject in
       fun ((score, keyboard) : Scored_keyboard.t) ->
         let%bind.Ui_effect () = window_insert (score, keyboard) in
         keyboard_inject [ Keyboard.Action.Random_swap ])
    graph
;;

let greedy
      ~corpus
      ~keyboard
      ~keyboard_inject
      ~window_insert
      ~metrics
      ~worst_counter
      ~diff_row_bigram_data
      ~score
      graph
  =
  let all_swaps = Keyboard.all_swaps keyboard graph in
  let id_pairs_to_score_and_keyboard =
    Bonsai.assoc
      (module Key.Id.Pair)
      all_swaps
      ~f:(fun _keypair keyboard graph ->
        let%arr keyboard = keyboard
        and score = score keyboard in
        score, keyboard)
      graph
  in
  let min_score =
    Bonsai.Map.min_value
      id_pairs_to_score_and_keyboard
      ~comparator:(module Scored_keyboard)
      graph
  in
  let curr_score = score keyboard in
  Bonsai.Edge.on_change
    ~equal:(Option.equal Scored_keyboard.equal)
    min_score
    ~callback:
      (let%arr curr_score = curr_score
       and keyboard = keyboard
       and window_insert = window_insert
       and keyboard_inject = keyboard_inject in
       fun (min_score : Scored_keyboard.t option) ->
         let next_effect = [ Keyboard.Action.Random_swap ] in
         match min_score with
         | None ->
           printf "curr / min: %.2f / None\n%!" curr_score;
           keyboard_inject next_effect
         | Some (min_score, min_keyboard) ->
           printf "curr / min: %.2f / Some(%.2f)\n%!" curr_score min_score;
           if Float.( < ) min_score curr_score
           then (
             let%bind.Ui_effect () = window_insert (min_score, min_keyboard) in
             let overwrite = Map.map min_keyboard ~f:(fun (key : Key.t) -> key.kc) in
             keyboard_inject [ Overwrite overwrite ])
           else if Scored_keyboard.equal (min_score, min_keyboard) (curr_score, keyboard)
           then
             keyboard_inject (Core.List.init 30 ~f:(fun _ -> Keyboard.Action.Random_swap))
           else keyboard_inject [ Random_swap ])
    graph
;;

let component ~action ~corpus graph =
  let keyboard, keyboard_inject, _cancel = Keyboard.state_machine graph in
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
  let diff_row_bigram_data keyboard =
    let data = Bigram_data.make keyboard corpus graph in
    Stats_same_finger.bigram_data data graph
  in
  let window, window_insert, window_reset =
    let window, window_insert, window_reset =
      Window.descending ~size:(Bonsai.return 10) ~compare:Float.compare graph
    in
    let window =
      let%arr window = window in
      let res = List.rev window in
      List.iter res ~f:(fun (score, _keyboard) -> printf "list: %.2f\n" score);
      res
    in
    window, window_insert, window_reset
  in
  let score keyboard =
    let%arr score =
      score
        ~keyboard
        ~metrics
        ~worst_counter
        ~diff_row_bigram_data:(diff_row_bigram_data keyboard)
        graph
    in
    Option.value score ~default:Float.max_value
  in
  (ignore : unit Bonsai.t -> unit)
    (match%sub [%lazy] action with
     | Greedy ->
       greedy
         ~corpus
         ~keyboard
         ~keyboard_inject
         ~window_insert
         ~metrics
         ~worst_counter
         ~diff_row_bigram_data
         ~score
         graph;
       Bonsai.return ()
     | Random ->
       random
         ~corpus
         ~keyboard
         ~keyboard_inject
         ~window_insert
         ~metrics
         ~worst_counter
         ~diff_row_bigram_data
         ~score
         graph;
       Bonsai.return ());
  keyboard, window, window_reset
;;
