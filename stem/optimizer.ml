open! Import

type t =
  | Random
  | Greedy
[@@deriving sexp, compare, equal, enumerate, bin_io]

let default = Greedy

(* let score kind ~keyboard ~corpus graph = *)
(*   match%sub kind with *)
(*   | Greedy -> Score.greedy_descend keyboard ~corpus graph *)
(*   | Random -> Score.greedy_descend keyboard ~corpus graph *)
(* ;; *)

module Scored_keyboard = struct
  module T = struct
    type t = float * Keyboard.t [@@deriving sexp, compare, equal]
  end

  include T
  include Comparator.Make (T)
end

let epsilon = Float.epsilon_float
let i = ref 0
let j = ref 0
(* let greedy ~keyboard ~corpus graph = Score.greedy_descend keyboard ~corpus graph *)

(* Bonsai.Edge.on_change *)
(*   ~equal:(Option.equal Scored_keyboard.equal) *)
(*   min_scoring *)
(*   ~callback: *)
(*     (let%arr curr_score = curr_score *)
(*      and window_insert = window_insert *)
(*      and keyboard_inject = keyboard_inject in *)
(*      fun (min_score : Scored_keyboard.t option) -> *)
(*        printf "%d\n%!" !i; *)
(*        incr i; *)
(*        let next_effect = [ Keyboard.Action.Random_swap ] in *)
(*        match min_score with *)
(*        | None -> keyboard_inject next_effect *)
(*        | Some (min_score, min_keyboard) -> *)
(*          let delta = Float.abs (1. -. (curr_score /. min_score)) in *)
(*          let epsilon = 100. *. Float.epsilon_float in *)
(*          let reset_condition = Float.( < ) delta epsilon in *)
(*          printf *)
(*            "delta: %.20f ___ 10. *. epsilon: %.20f ___ reset: %b \n%!" *)
(*            delta *)
(*            epsilon *)
(*            reset_condition; *)
(*          if not reset_condition *)
(*          then ( *)
(*            let%bind.Ui_effect () = window_insert (min_score, min_keyboard) in *)
(*            let overwrite = Map.map min_keyboard ~f:(fun (key : Key.t) -> key.kc) in *)
(*            keyboard_inject [ Overwrite overwrite ]) *)
(*          else ( *)
(*            printf "Random 30\n%!"; *)
(*            keyboard_inject (Core.List.init 30 ~f:(fun _ -> Keyboard.Action.Random_swap)))) *)
(*   graph *)

let m = ref 0

let component ~keyboard ~keyboard_inject ~live_keyboard_inject ~action ~corpus graph =
  let window, window_insert, window_reset =
    let window, window_insert, window_reset =
      Window.descending ~size:(Bonsai.return 10) ~compare:Float.compare graph
    in
    let window =
      let%arr window = window in
      List.rev window
    in
    window, window_insert, window_reset
  in
  (* let _score keyboard = score (Bonsai.return Greedy) ~keyboard ~corpus graph in *)
  let tick =
    match%sub [%lazy] action with
    | Greedy ->
      let scored_keyboard =
        Score.greedy_descend ~input_keyboard:keyboard ~live_keyboard_inject ~corpus graph
      in
      let () =
        Bonsai.Edge.on_change
          ~equal:(Option.equal Scored_keyboard.equal)
          scored_keyboard
          ~callback:
            (let%arr keyboard_inject = keyboard_inject
             and window_insert = window_insert in
             fun (score_keyboard : Scored_keyboard.t option) ->
               printf "on_change_handler = %d\n%!" !m;
               incr m;
               match score_keyboard with
               | None -> Ui_effect.Ignore
               | Some (score, keyboard) ->
                 print_endline (Keyboard.to_string keyboard);
                 let%bind.Ui_effect () = window_insert (score, keyboard) in
                 keyboard_inject
                   [ Keyboard.Action.Overwrite
                       (Map.map keyboard ~f:(fun (key : Key.t) -> key.kc))
                   ])
          graph
      in
      Bonsai.return Ui_effect.Ignore
    | Random ->
      (* let scored_keyboard = score keyboard in *)
      (* let  () = on_change_handler scored_keyboard in *)
      (* let random_swap = *)
      (*   let%arr keyboard_inject = keyboard_inject in *)
      (*   keyboard_inject [ Keyboard.Action.Random_swap ] *)
      (* in *)
      (* Bonsai.Edge.after_display random_swap graph; *)
      Bonsai.return Ui_effect.Ignore
  in
  keyboard, window, window_reset, tick
;;
