open! Import

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

let random ~corpus graph =
  let t, keyboard_inject, _cancel = Keyboard.state_machine graph in
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
  Bonsai.Edge.on_change
    ~equal:Scored_keyboard.equal
    (Bonsai.both (score t) t)
    ~callback:
      (let%arr window_insert = window_insert
       and keyboard_inject = keyboard_inject in
       fun ((score, t) : Scored_keyboard.t) ->
         let%bind.Ui_effect () = window_insert (score, t) in
         keyboard_inject [ Random_swap ])
    graph;
  t, window, window_reset
;;

let greedy ~corpus graph =
  let t, keyboard_inject, _cancel = Keyboard.state_machine graph in
  let all_swaps = Keyboard.all_swaps t graph in
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
  let id_pairs_to_score_and_keyboard =
    Bonsai.assoc
      (module Key.Id.Pair)
      all_swaps
      ~f:(fun _keypair keyboard graph ->
        let%arr keyboard = keyboard
        and score =
          score
            ~keyboard
            ~metrics
            ~worst_counter
            ~diff_row_bigram_data:(diff_row_bigram_data keyboard)
            graph
        in
        (* printf *)
        (*   "keyboard:%s\n%s\n" *)
        (*   (match score with *)
        (*    | None -> "None" *)
        (*    | Some score -> sprintf "Some(%.2f)" score) *)
        (*   (Keyboard.to_string keyboard); *)
        Option.value_exn score, keyboard)
      graph
  in
  let min_score =
    Bonsai.Map.min_value
      id_pairs_to_score_and_keyboard
      ~comparator:(module Scored_keyboard)
      graph
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
  let curr_score =
    let%arr score =
      score
        ~keyboard:t
        ~metrics
        ~worst_counter
        ~diff_row_bigram_data:(diff_row_bigram_data t)
        graph
    in
    Option.value score ~default:Float.max_value
  in
  Bonsai.Edge.on_change
    ~equal:(Option.equal Scored_keyboard.equal)
    min_score
    ~callback:
      (let%arr curr_score = curr_score
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
             keyboard_inject (Overwrite overwrite :: next_effect))
           else keyboard_inject next_effect)
    graph;
  t, window, window_reset
;;

let component ~action ~corpus graph =
  match%sub action with
  | Optimizer.Greedy ->
    let t, window, window_reset = greedy ~corpus graph in
    let%arr t = t
    and window = window
    and window_reset = window_reset in
    t, window, window_reset
  | Random ->
    let t, window, window_reset = random ~corpus graph in
    let%arr t = t
    and window = window
    and window_reset = window_reset in
    t, window, window_reset
;;
