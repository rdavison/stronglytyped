open! Import
include Gen_intf

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

  type t =
    { final_score : Score.t
    ; save_state : Layout.save_state
    }

  let acceptance_probability old_cost new_cost temperature ~score_scalarize ~score_compare
    =
    let res =
      if score_compare new_cost old_cost < 0
      then 1.
      else (
        let s_old_cost = score_scalarize old_cost in
        let s_new_cost = score_scalarize new_cost in
        if Float.( < ) s_new_cost s_old_cost
        then 0.
        else
          (* Float.exp ((old_cost -. new_cost) /. temperature) *)
          Float.exp
            ((s_old_cost -. s_new_cost)
             /. (temperature
                 *. ((1.
                      /. 3.
                      *. (Float.cos ((1_000_000. *. temperature) -. (2. *. Float.pi))
                          +. 1.))
                     +. 1.))))
    in
    (* if new_cost < old_cost && Random.int 10 = 1
       then
       printf
       "%.20f\t%.20f\t%.12f\n%!"
       new_cost
       ((old_cost -. new_cost) /. temperature)
       temperature; *)
    res
  ;;

  let simulated_annealing
    ~(objective_function : Layout.save_state -> Score.t)
    ~score_compare
    ~score_scalarize
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
    let force_stop = ref 0 in
    (* let max_force_stop = ref !force_stop in *)
    let iteration = ref 0 in
    while !iteration < num_iterations - 1 || !force_stop > 80_000 do
      let ( > ) = Float.( > ) in
      (* let ( < ) = Float.( < ) in *)
      let new_solution = make_next_solution !current_solution in
      let new_cost = objective_function new_solution in
      if acceptance_probability
           !current_cost
           new_cost
           !temperature
           ~score_compare
           ~score_scalarize
         > Random.float 1.
      then (
        current_solution := new_solution;
        current_cost := new_cost);
      if score_compare new_cost !best_cost < 0
      then (
        (* printf
           "%f\t%d/%d\t%.12f\t%d\n%!"
           new_cost
           !iteration
           num_iterations
           !temperature
           !force_stop; *)
        (* if Int.( > ) !force_stop !max_force_stop then max_force_stop := !force_stop; *)
        force_stop := 0;
        best_solution := new_solution;
        best_cost := new_cost)
      else incr force_stop;
      temperature := !temperature *. cooling_rate;
      incr iteration
    done;
    (* printf "max_force_stop = %d\n%!" !max_force_stop; *)
    !best_solution, !best_cost
  ;;

  let anneal
    (layout : Layout.t)
    ~score_obs
    ~score_compare
    ~score_scalarize
    ~initial_temperature
    ~cooling_rate
    ~num_iterations
    =
    let layout_info = Layout.info_of_id layout.id in
    let observer = score_obs in
    let make_next_solution save_state =
      Layout.load layout save_state;
      let swaps = ref [] in
      while List.is_empty !swaps do
        let a, b = Random.int layout_info.num_keys, Random.int layout_info.num_keys in
        swaps := Layout.swaps layout a b
      done;
      Layout.swap !swaps;
      Layout.save layout
    in
    let objective_function (save_state : Layout.save_state) =
      Layout.load layout save_state;
      Incr.stabilize ();
      let score : Score.t = Incr.Observer.value_exn observer in
      score
    in
    let initial_solution = Layout.save layout in
    let best_solution, best_cost =
      simulated_annealing
        ~objective_function
        ~score_compare
        ~score_scalarize
        ~make_next_solution
        ~initial_solution
        ~initial_temperature
        ~cooling_rate
        ~num_iterations
    in
    { final_score = best_cost; save_state = best_solution }
  ;;

  let bruteforce (layout : Layout.t) ~score_obs ~score_compare ~score_scalarize ~mode =
    let layout_info = Layout.info_of_id layout.id in
    let module SS = struct
      module T = struct
        type t = Layout.save_state [@@deriving sexp, compare, equal]
      end

      include T
      include Comparable.Make (T)
    end
    in
    let observer = score_obs in
    Incr.stabilize ();
    let og_save_state = Layout.save layout in
    let best_save_state = ref og_save_state in
    let best_score = ref (Incr.Observer.value_exn observer) in
    let iteration = ref 0 in
    let continue_ = ref true in
    while !continue_ do
      continue_ := false;
      incr iteration;
      printf "Brute Force Round %d...\n%!" !iteration;
      let save_states =
        let seen = ref SS.Set.empty in
        (match mode with
         | `Slow ->
           for i1 = 0 to layout_info.num_keys - 1 do
             for j1 = i1 + 1 to layout_info.num_keys - 2 do
               for i2 = 0 to layout_info.num_keys - 1 do
                 for j2 = i2 + 1 to layout_info.num_keys - 2 do
                   let sw1 = Layout.swaps layout i1 j1 in
                   let sw2 = Layout.swaps layout i2 j2 in
                   let swaps = sw1 @ sw2 in
                   Layout.swap swaps;
                   seen := Set.add !seen (Layout.save layout);
                   Layout.swap (List.rev swaps)
                 done
               done
             done
           done
         | `Fast ->
           for i1 = 0 to layout_info.num_keys - 1 do
             for j1 = i1 + 1 to layout_info.num_keys - 2 do
               let sw1 = Layout.swaps layout i1 j1 in
               let swaps = sw1 in
               Layout.swap swaps;
               seen := Set.add !seen (Layout.save layout);
               Layout.swap (List.rev swaps)
             done
           done);
        Set.to_list !seen
      in
      printf "Seen: %d...\n%!" (List.length save_states);
      let current_best_score, current_best_save_state =
        save_states
        |> List.mapi ~f:(fun _i save_state ->
          Layout.load layout save_state;
          Incr.stabilize ();
          (* printf "%d\n%!" i; *)
          let score = Incr.Observer.value_exn observer in
          (* printf "%f\n%!" score; *)
          score, save_state)
        |> List.sort ~compare:(fun (a, _) (b, _) -> score_compare a b)
        |> List.hd_exn
      in
      if score_compare current_best_score !best_score < 0
      then (
        continue_ := true;
        best_save_state := current_best_save_state;
        best_score := current_best_score;
        printf
          "Brute Force Round %d Improvement...\n%f\n%s\n%!"
          !iteration
          (score_scalarize !best_score)
          (Layout.pretty_string layout));
      Layout.load layout !best_save_state
    done;
    Incr.stabilize ();
    (* printf "Done brute forcing...\n%!"; *)
    { final_score = Incr.Observer.value_exn observer; save_state = Layout.save layout }
  ;;
end
