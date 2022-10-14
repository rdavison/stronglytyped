open! Import
open! Stronglytyped_analyzer

module Default = struct
  let ksize = 30
  let detailed_output = true
  let max_runs = Int.max_value
  let chance_exponentiator = 0.9
  let runs_before_gtb_rounds_increase = 4
  let algorithm_rounds = 16
  let chance_to_use_previous_layout = 0.2
  let num_swaps_between_rounds = ksize / 15
  let runs_before_chance_increases = 1
  let runs_before_swaps_increase = 1
  let gtb_rounds = 4
  let print_time_interval = Time.Span.of_sec 60.
  let fitness_max = Float.max_value
  let gtb_number_of_swaps = 10
  let gtb_rounds_before_swaps_increase = 32
end

let runs_before_chance_inc_v = Incr.Var.create Default.runs_before_chance_increases
let runs_before_chance_inc = Incr.Var.watch runs_before_chance_inc_v
let runs_before_swaps_inc_v = Incr.Var.create Default.runs_before_swaps_increase
let runs_before_swaps_inc = Incr.Var.watch runs_before_swaps_inc_v
let run_v = Incr.Var.create 0
let run = Incr.Var.watch run_v
let chance_to_use_prev_v = Incr.Var.create Default.chance_to_use_previous_layout
let chance_to_use_prev = Incr.Var.watch chance_to_use_prev_v
let swaps_v = Incr.Var.create Default.num_swaps_between_rounds
let swaps = Incr.Var.watch swaps_v
let gtb_rounds_v = Incr.Var.create Default.gtb_rounds
let gtb_rounds = Incr.Var.watch gtb_rounds_v
