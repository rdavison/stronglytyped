open! Import

let acceptance_probability old_cost new_cost temperature =
  let ( < ) = Float.( < ) in
  let res =
    if new_cost < old_cost then 1. else Float.exp ((old_cost -. new_cost) /. temperature)
  in
  if Random.float 1. < 0.001
  then printf "%.4f\t%.4f\t%.4f\t%f\n%!" old_cost new_cost res temperature;
  res
;;

let simulated_annealing
  ~objective_function
  ~make_next_solution
  ~(initial_solution : Layout.t)
  ~initial_temperature
  ~cooling_rate
  ~num_iterations
  =
  let current_solution = ref initial_solution in
  let best_solution = ref initial_solution in
  let current_cost = ref (objective_function !current_solution) in
  let best_cost = ref !current_cost in
  let temperature = ref initial_temperature in
  let ( > ) = Float.( > ) in
  let ( < ) = Float.( < ) in
  for _ = 0 to num_iterations - 1 do
    let new_solution = make_next_solution !current_solution in
    let new_cost = objective_function new_solution in
    if acceptance_probability !current_cost new_cost !temperature > Random.float 1.
    then (
      current_solution := new_solution;
      current_cost := new_cost);
    if new_cost < !best_cost
    then (
      best_solution := new_solution;
      best_cost := new_cost);
    temperature := !temperature *. cooling_rate
  done;
  !best_solution, !best_cost
;;

let run layout ~corpus ~weights =
  let next_swap =
    let valid_swaps = Layout.valid_swaps layout in
    let len = Array.length valid_swaps in
    fun () ->
      let i = Random.int len in
      valid_swaps.(i)
  in
  let stats = Stats.make layout corpus in
  let score = Score.make stats ~weights in
  let observer = Incr.observe score in
  let make_next_solution current =
    for _ = 1 to Random.int 3 do
      let a, b = next_swap () in
      Layout.swap current a b
    done;
    current
  in
  let objective_function (_ : Layout.t) =
    Incr.stabilize ();
    let score = Incr.Observer.value_exn observer in
    (* printf "score %f\n" score; *)
    score
  in
  let initial_solution = layout in
  let initial_temperature = 100.0 in
  let cooling_rate = 0.99998 in
  let num_iterations = 1000000 in
  let best_solution, best_cost =
    simulated_annealing
      ~objective_function
      ~make_next_solution
      ~initial_solution
      ~initial_temperature
      ~cooling_rate
      ~num_iterations
  in
  best_cost, best_solution
;;