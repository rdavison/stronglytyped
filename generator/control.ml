open! Import
open! Stronglytyped_analyzer

type t =
  { chance_to_use_prev : float
  ; swaps : int
  ; gtb_rounds : int
  }

let chance_to_use_prev =
  let%bind.Incr run = Vars.run
  and runs_before_chance_inc = Vars.runs_before_chance_inc in
  match run mod runs_before_chance_inc = 0 with
  | false -> Vars.chance_to_use_prev
  | true ->
    let%map.Incr chance_to_use_prev = Vars.chance_to_use_prev in
    let chance_to_use_prev = chance_to_use_prev ** Vars.Default.chance_exponentiator in
    let runs_before_chance_inc =
      Int.of_float (Float.of_int runs_before_chance_inc *. 1.2) + 1
    in
    Incr.Var.set Vars.chance_to_use_prev_v chance_to_use_prev;
    Incr.Var.set Vars.runs_before_chance_inc_v runs_before_chance_inc;
    if Vars.Default.detailed_output
    then printf "Chance to use previous layout is now %f.\n" chance_to_use_prev;
    chance_to_use_prev
;;

let swaps =
  let%bind.Incr run = Vars.run
  and runs_before_swaps_inc = Vars.runs_before_swaps_inc in
  match run mod runs_before_swaps_inc = 0 with
  | false -> Vars.swaps
  | true ->
    let%map.Incr swaps = Vars.swaps in
    let swaps = swaps + 1 in
    let runs_before_swaps_inc =
      Int.of_float (Float.of_int runs_before_swaps_inc *. 1.2) + 1
    in
    Incr.Var.set Vars.swaps_v swaps;
    Incr.Var.set Vars.runs_before_swaps_inc_v runs_before_swaps_inc;
    if Vars.Default.detailed_output
    then printf "Number of swaps between rounds is now %d.\n" swaps;
    swaps
;;

let gtb_rounds =
  let%bind.Incr run = Vars.run in
  match run mod Vars.Default.runs_before_gtb_rounds_increase = 0 with
  | false -> Vars.gtb_rounds
  | true ->
    let%map.Incr gtb_rounds = Vars.gtb_rounds in
    let gtb_rounds = gtb_rounds * 2 in
    Incr.Var.set Vars.gtb_rounds_v gtb_rounds;
    if Vars.Default.detailed_output
    then printf "Number of rounds in greatToBest() is now %d.\n" gtb_rounds;
    gtb_rounds
;;

let incr =
  let%map_open.Incr chance_to_use_prev = chance_to_use_prev
  and swaps = swaps
  and gtb_rounds = gtb_rounds in
  { chance_to_use_prev; swaps; gtb_rounds }
;;

let observer = Incr.observe incr

let reset () =
  Incr.Var.set Vars.runs_before_chance_inc_v Vars.Default.runs_before_chance_increases;
  Incr.Var.set Vars.runs_before_swaps_inc_v Vars.Default.runs_before_swaps_increase
;;