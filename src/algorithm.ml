open! Import
include Algorithm_intf

module Make
    (Incr : Incremental.S)
    (Layout : Layout.S with module Incr = Incr)
    (Stats : Stats.S with module Incr = Incr and module Layout = Layout)
    (Score : Score.S
               with module Incr = Incr
                and module Layout = Layout
                and module Stats = Stats) =
struct
  module Incr = Incr
  module Layout = Layout
  module Stats = Stats
  module Score = Score

  let acceptance_probability old_cost new_cost temperature =
    let ( < ) = Float.( < ) in
    let res =
      if new_cost < old_cost then 1. else Float.exp ((old_cost -. new_cost) /. temperature)
    in
    (* if new_cost < old_cost
       then
       printf
       "%.20f\t%.20f\t%.12f\n%!"
       new_cost
       ((old_cost -. new_cost) /. temperature)
       temperature; *)
    res
  ;;

  let simulated_annealing
    ~objective_function
    ~make_next_solution
    ~(initial_solution : Layout.save_state)
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

  let anneal layout ~corpus ~score =
    let stats = Stats.make layout corpus in
    let score = score stats in
    let final_sum = Score.final_sum score in
    let observer = Incr.observe final_sum in
    let make_next_solution save_state =
      Layout.load layout save_state;
      let swaps = ref [] in
      while List.is_empty !swaps do
        let a, b = Random.int layout.num_keys, Random.int layout.num_keys in
        swaps := Layout.swaps layout a b
      done;
      Layout.swap !swaps;
      Layout.save layout
    in
    let objective_function (save_state : Layout.save_state) =
      Layout.load layout save_state;
      Incr.stabilize ();
      let score = Incr.Observer.value_exn observer in
      score
    in
    let initial_solution = Layout.save layout in
    let initial_temperature = 100.0 in
    let cooling_rate = 0.99997 in
    let num_iterations = 1_000_000 in
    (* let num_iterations = 0 in *)
    let best_solution, best_cost =
      simulated_annealing
        ~objective_function
        ~make_next_solution
        ~initial_solution
        ~initial_temperature
        ~cooling_rate
        ~num_iterations
    in
    best_cost, stats, score, best_solution
  ;;

  type t =
    { score : float
    ; pretty : string
    ; save_state : Layout.save_state
    }

  let bruteforce (layout : Layout.t) ~corpus ~score =
    let module SS = struct
      module T = struct
        type t = Layout.save_state [@@deriving sexp, compare, equal]
      end

      include T
      include Comparable.Make (T)
    end
    in
    let stats = Stats.make layout corpus in
    let score = score stats in
    let observer = Incr.observe score in
    Incr.stabilize ();
    let best_save_state = ref (Layout.save layout) in
    let best_score = ref (Incr.Observer.value_exn observer) in
    let iteration = ref 0 in
    let continue_ = ref true in
    while !continue_ do
      continue_ := false;
      incr iteration;
      printf "Brute Force Round %d...\n%!" !iteration;
      let save_states =
        let seen = ref SS.Set.empty in
        for i = 0 to layout.num_keys - 2 do
          for j = i + 1 to layout.num_keys - 1 do
            let swaps = Layout.swaps layout i j in
            Layout.swap swaps;
            seen := Set.add !seen (Layout.save layout);
            Layout.swap (List.rev swaps)
          done
        done;
        Set.to_list !seen
      in
      let current_best_score, current_best_save_state =
        (* TODO: Figure out how to parallelize this. *)
        save_states
        |> List.map ~f:(fun save_state ->
          Layout.load layout save_state;
          Incr.stabilize ();
          let score = Incr.Observer.value_exn observer in
          score, save_state)
        |> List.sort ~compare:(fun (a, _) (b, _) -> Float.compare a b * -1)
        |> List.hd_exn
      in
      if Float.(current_best_score < !best_score)
      then (
        continue_ := true;
        best_save_state := current_best_save_state;
        best_score := current_best_score;
        printf
          "Brute Force Round %d Improvement...\n%f\n%s\n%!"
          !iteration
          !best_score
          (Layout.pretty_string layout));
      Layout.load layout !best_save_state
    done;
    Incr.stabilize ();
    printf "Done brute forcing...\n%!";
    { pretty = Layout.pretty_string layout
    ; score = Incr.Observer.value_exn observer
    ; save_state = Layout.save layout
    }
  ;;
end
