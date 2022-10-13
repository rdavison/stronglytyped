open! Import

module Default = struct
  let ksize = 30
  let detailedOutput = true
  let maxRuns = Int.max_value
  let chanceExponentiator = 0.9
  let runsBeforeGtbRoundsInc = 4
  let algorithm_rounds = 16
  let chanceToUsePreviousLayout = 0.2
  let num_swaps_between_rounds = ksize / 15
  let runsBeforeChanceInc = 1
  let runsBeforeSwapsInc = 1
  let gtbRounds = 4
  let print_time_interval = Time.Span.of_sec 60.
  let fitnessMax = Float.max_value
  let gtbNumberOfSwaps = 10
  let gtbRoundsBeforeSwapInc = 32
end

let runs_before_chance_inc_v = Incr.Var.create Default.runsBeforeChanceInc
let runs_before_chance_inc = Incr.Var.watch runs_before_chance_inc_v
let runs_before_swaps_inc_v = Incr.Var.create Default.runsBeforeSwapsInc
let runs_before_swaps_inc = Incr.Var.watch runs_before_swaps_inc_v
let run_v = Incr.Var.create 0
let run = Incr.Var.watch run_v
let p_use_prev_v = Incr.Var.create Default.chanceToUsePreviousLayout
let p_use_prev = Incr.Var.watch p_use_prev_v
let swaps_v = Incr.Var.create Default.num_swaps_between_rounds
let swaps = Incr.Var.watch swaps_v
let gtb_rounds_v = Incr.Var.create Default.gtbRounds
let gtb_rounds = Incr.Var.watch gtb_rounds_v

module Control = struct
  type t =
    { chance_to_use_prev : float
    ; swaps : int
    ; gtb_rounds : int
    }

  let reset () =
    Incr.Var.set runs_before_chance_inc_v Default.runsBeforeChanceInc;
    Incr.Var.set runs_before_swaps_inc_v Default.runsBeforeSwapsInc
  ;;

  let chance_to_use_prev =
    let%bind.Incr run = run
    and runs_before_chance_inc = runs_before_chance_inc in
    match run mod runs_before_chance_inc = 0 with
    | false -> p_use_prev
    | true ->
      let%map.Incr p_use_prev = p_use_prev in
      let p_use_prev = p_use_prev ** Default.chanceExponentiator in
      let runs_before_chance_inc =
        Int.of_float (Float.of_int runs_before_chance_inc *. 1.2) + 1
      in
      Incr.Var.set p_use_prev_v p_use_prev;
      Incr.Var.set runs_before_chance_inc_v runs_before_chance_inc;
      if Default.detailedOutput
      then printf "Chance to use previous layout is now %f.\n" p_use_prev;
      p_use_prev
  ;;

  let swaps =
    let%bind.Incr run = run
    and runs_before_swaps_inc = runs_before_swaps_inc in
    match run mod runs_before_swaps_inc = 0 with
    | false -> swaps
    | true ->
      let%map.Incr swaps = swaps in
      let swaps = swaps + 1 in
      let runs_before_swaps_inc =
        Int.of_float (Float.of_int runs_before_swaps_inc *. 1.2) + 1
      in
      Incr.Var.set swaps_v swaps;
      Incr.Var.set runs_before_swaps_inc_v runs_before_swaps_inc;
      if Default.detailedOutput
      then printf "Number of swaps between rounds is now %d.\n" swaps;
      swaps
  ;;

  let gtb_rounds =
    let%bind.Incr run = run in
    match run mod Default.runsBeforeGtbRoundsInc = 0 with
    | false -> gtb_rounds
    | true ->
      let%map.Incr gtb_rounds = gtb_rounds in
      let gtb_rounds = gtb_rounds * 2 in
      Incr.Var.set gtb_rounds_v gtb_rounds;
      if Default.detailedOutput
      then printf "Number of rounds in greatToBest() is now %d.\n" gtb_rounds;
      gtb_rounds
  ;;

  let incr =
    let%map_open.Incr chance_to_use_prev = chance_to_use_prev
    and swaps = swaps
    and gtb_rounds = gtb_rounds in
    { chance_to_use_prev; swaps; gtb_rounds }
  ;;
end

let analysis_o = Incr.observe Analysis.incr

let next () =
  Incr.stabilize ();
  Incr.Observer.value_exn analysis_o
;;

let nil () =
  Root.scramble 30;
  next ()
;;

let smart_mutate ~monograms_arr ~swaps ~reverse_lookup_table =
  let monLen = Array.length monograms_arr in
  let q = monLen / 4 in
  let swapslen = 2 * swaps in
  let charsToSwap =
    Array.init swapslen ~f:(fun _ ->
        let rec loop i default =
          match i < monLen with
          | false -> default
          | true ->
            (match Random.int Int.max_value mod q = 0 with
            | false -> loop (i + 1) default
            | true -> fst monograms_arr.(i))
        in
        loop 0 (fst monograms_arr.(0)))
  in
  let lockins =
    let rec loop i lockins =
      if i < swapslen
      then lockins
      else (
        let lc1 = Map.find_exn reverse_lookup_table charsToSwap.(i) in
        let lc2 = Map.find_exn reverse_lookup_table charsToSwap.(i + 1) in
        let lockins = (lc1, lc2) :: lockins in
        Root.swap lc1 lc2;
        loop (i + 2) lockins)
    in
    loop 0 []
  in
  next (), Array.of_list_rev lockins
;;

let buildShuffledIndices length =
  let indices = Array.init length ~f:(Fn.const 0) in
  for i = 0 to length - 1 do
    let j = Random.int Int.max_value mod (i + 1) in
    indices.(i) <- indices.(j);
    indices.(j) <- i
  done;
  indices
;;

let improveLayout (evaluationToBeat : float) lockins =
  let ksize = Default.ksize in
  let indices = buildShuffledIndices ksize in
  let rec loop_i i =
    let rec loop_j j =
      match j < ksize with
      | false -> `Continue
      | true ->
        let skipRound =
          let rec loop inx acc =
            match inx < Array.length lockins with
            | false -> acc
            | true ->
              if fst lockins.(inx) = indices.(i)
                 || fst lockins.(inx) = indices.(j)
                 || snd lockins.(inx) = indices.(i)
                 || snd lockins.(inx) = indices.(j)
              then true
              else loop (inx + 1) acc
          in
          loop 0 false
        in
        (match skipRound with
        | true -> loop_j (j + 1)
        | false ->
          let (k : Analysis.t) =
            Root.swap indices.(i) indices.(j);
            next ()
          in
          let evaluation = k.score in
          if Float.(evaluation < evaluationToBeat)
          then `Stop k
          else (
            (* Undo swap*)
            Root.swap indices.(i) indices.(j);
            loop_j (j + 1)))
    in
    match i < ksize with
    | false -> None
    | true ->
      (match loop_j (i + 1) with
      | `Stop x -> Some x
      | `Continue -> loop_i (i + 1))
  in
  loop_i 0
;;

let anneal (k : Analysis.t) lockins =
  let rec loop (lastEvaluation : Analysis.t) (evaluation : Analysis.t) =
    let lastImprovement =
      if Float.(evaluation.score < lastEvaluation.score)
      then lastEvaluation.score -. evaluation.score
      else 0.
    in
    let lastEvaluation = evaluation in
    let evaluationToBeat = lastEvaluation.score +. lastImprovement in
    let evaluation' = improveLayout evaluationToBeat lockins in
    match evaluation' with
    | None -> evaluation
    | Some evaluation' ->
      if Float.(evaluation'.score < evaluationToBeat)
      then loop lastEvaluation evaluation'
      else evaluation
  in
  loop k k
;;

let print_percentages (k : Analysis.t) =
  let layout_pretty = k.layout_pretty in
  let stats = k.stats in
  let res =
    String.concat
      ~sep:"\n"
      (List.filter_opt
         [ Some layout_pretty
         ; Some "---------------------"
         ; Option.map stats ~f:Stats.to_string
         ; Some (sprintf "Score: %.10f" k.score)
         ])
  in
  print_endline res
;;

let print_time start_time =
  let end_time = Time.now () in
  let span = Time.diff end_time start_time in
  printf "Time elapsed: %s\n" (Time.Span.to_string_hum span)
;;

let smart_mutate_and_anneal
    ~p_use_prev
    ~swaps
    ~start_time
    ~num_rounds
    ~bestk
    ~monograms_arr
    ~reverse_lookup_table
  =
  let rec loop i (prevk, bestk) =
    match i < num_rounds with
    | false -> bestk
    | true ->
      let prevk' =
        if i > 0 && Float.(Random.float 1. < p_use_prev)
        then fst (smart_mutate ~monograms_arr ~swaps ~reverse_lookup_table)
        else nil ()
      in
      let k = anneal prevk [||] in
      let bestk' =
        if Float.(k.score < bestk.Analysis.score)
        then (
          print_percentages k;
          print_time start_time;
          k)
        else bestk
      in
      loop (i + 1) (prevk', bestk')
  in
  let bestk' = loop 0 (nil (), bestk) in
  if Float.(bestk'.score < bestk.score) then bestk' else bestk
;;

type run =
  { n : int
  ; bestk : Analysis.t option
  ; prev_best_fitness : float
  ; start_time : Time.t
  ; print_time_interval : Time.Span.t
  ; time_on_print : Time.Span.t
  }

type mode =
  | Stop
  | Reset
  | Ready
  | Run of run

let mode_v = Incr.Var.create Reset
let mode = Incr.Var.watch mode_v
let mode_o = Incr.observe mode

let reset () =
  Layout.set (`Name "dvorak");
  Root.scramble 30;
  Control.reset ();
  Incr.Var.set mode_v Ready
;;

let get_user_input () =
  let start_time = Time.now () in
  let print_time_interval = Default.print_time_interval in
  let time_on_print =
    Time.Span.( + ) (start_time |> Time.to_span_since_epoch) print_time_interval
  in
  (* Fake user input *)
  Incr.Var.set
    mode_v
    (Run
       { n = 1
       ; bestk = None
       ; prev_best_fitness = Default.fitnessMax
       ; start_time
       ; print_time_interval
       ; time_on_print
       })
;;

let update_miscellaneous
    ~bestk
    ~prev_best_fitness
    ~start_time
    ~print_time_interval
    ~time_on_print
  =
  let is_better = Float.(bestk.Analysis.score < prev_best_fitness) in
  let ts = Time.now () |> Time.to_span_since_epoch in
  let time_on_print' =
    let ( + ) = Time.Span.( + ) in
    ts + print_time_interval
  in
  if is_better
  then (
    let prev_best_fitness = bestk.score in
    print_percentages bestk;
    print_time start_time;
    (* If a keyboard was just printed, don't print the time for a while. *)
    prev_best_fitness, time_on_print', print_time_interval)
  else if Time.Span.(ts >= time_on_print) && Default.detailedOutput
  then (
    print_time start_time;
    let print_time_interval =
      (Time.Span.to_sec print_time_interval *. 1.5) +. 1. |> Time.Span.of_sec
    in
    prev_best_fitness, time_on_print', print_time_interval)
  else prev_best_fitness, time_on_print, print_time_interval
;;

let great_to_best (bestk : Analysis.t) ~num_rounds ~monograms_arr ~reverse_lookup_table =
  let rec loop i ~(bestk : Analysis.t) ~swaps =
    match i < num_rounds with
    | false -> bestk
    | true ->
      let swaps =
        if i mod Default.gtbRoundsBeforeSwapInc = Default.gtbRoundsBeforeSwapInc - 1
        then swaps + 1
        else swaps
      in
      (* Any swaps made by smartMutate() are "locked in" and may not be undone by anneal() *)
      let mutated, lockins = smart_mutate ~swaps ~monograms_arr ~reverse_lookup_table in
      let (k : Analysis.t) =
        (* Use lockins only half the time *)
        if i mod 2 = 0 then anneal mutated lockins else anneal mutated [||]
      in
      let bestk = if Float.(k.score < bestk.score) then k else bestk in
      loop (i + 1) ~bestk ~swaps
  in
  let (bestk' : Analysis.t) = loop 0 ~bestk ~swaps:Default.gtbNumberOfSwaps in
  if Float.(bestk'.score < bestk.score) then bestk' else bestk
;;

let run
    { Control.chance_to_use_prev; swaps; gtb_rounds }
    monograms_arr
    reverse_lookup_table
    ~bestk
    ~prev_best_fitness
    ~start_time
    ~print_time_interval
    ~time_on_print
    ~num_rounds
  =
  let bestk =
    smart_mutate_and_anneal
      ~p_use_prev:chance_to_use_prev
      ~swaps
      ~start_time
      ~num_rounds
      ~bestk
      ~monograms_arr
      ~reverse_lookup_table
  in
  let prev_best_fitness, time_on_print, print_time_interval =
    update_miscellaneous
      ~bestk
      ~prev_best_fitness
      ~start_time
      ~print_time_interval
      ~time_on_print
  in
  let bestBeforeGTB = bestk.score in
  let (bestk : Analysis.t) =
    great_to_best bestk ~num_rounds:gtb_rounds ~monograms_arr ~reverse_lookup_table
  in
  let prev_best_fitness =
    if Float.(bestk.score < bestBeforeGTB)
    then (
      let prev_best_fitness = bestk.score in
      if Default.detailedOutput then printf "\n***Found from greatToBest()***\n";
      print_percentages bestk;
      print_time start_time;
      prev_best_fitness)
    else prev_best_fitness
  in
  bestk, prev_best_fitness, time_on_print, print_time_interval
;;

let game_loop () =
  reset ();
  let mode = Incr.observe mode in
  let control = Incr.observe Control.incr in
  let monograms_arr = Incr.observe Corpus.monograms_arr in
  let reverse_lookup_table = Incr.observe Root.reverse_lookup_table in
  let rec loop () =
    Incr.stabilize ();
    let mode = Incr.Observer.value_exn mode in
    match mode with
    | Stop -> ()
    | Reset ->
      reset ();
      Incr.Var.set mode_v Ready;
      loop ()
    | Ready ->
      get_user_input ();
      loop ()
    | Run { n; bestk; prev_best_fitness; start_time; print_time_interval; time_on_print }
      ->
      let bestk =
        match bestk with
        | None -> nil ()
        | Some bestk -> bestk
      in
      let control = Incr.Observer.value_exn control in
      let monograms_arr = Incr.Observer.value_exn monograms_arr in
      let reverse_lookup_table = Incr.Observer.value_exn reverse_lookup_table in
      Incr.Var.set run_v n;
      let bestk, prev_best_fitness, time_on_print, print_time_interval =
        run
          control
          monograms_arr
          reverse_lookup_table
          ~bestk
          ~prev_best_fitness
          ~start_time
          ~print_time_interval
          ~time_on_print
          ~num_rounds:Default.algorithm_rounds
      in
      let mode' =
        match n < Default.maxRuns with
        | false -> Stop
        | true ->
          Run
            { n = n + 1
            ; bestk = Some bestk
            ; prev_best_fitness
            ; start_time
            ; print_time_interval
            ; time_on_print
            }
      in
      Incr.Var.set mode_v mode';
      loop ()
  in
  loop ()
;;

let _main () =
  let generating () =
    match Incr.Observer.value_exn mode_o with
    | Run _ -> true
    | _ -> false
  in
  let counter = ref 1 in
  let attempts = 100 in
  let span, _ =
    time_it (fun () ->
        for _ = 1 to attempts do
          reset ();
          Incr.stabilize ();
          while generating () do
            incr counter;
            Incr.stabilize ()
          done
        done)
  in
  printf
    "Stabilized %d times. Time elapsed: %s. Stabilized %d per second.\n"
    !counter
    (Time.Span.to_string_hum span)
    (Float.of_int !counter /. Time.Span.to_sec span |> Float.to_int)
;;

let main = game_loop

let command =
  Command.basic
    ~summary:"Ypou Nercds"
    (let%map_open.Command () = return () in
     fun () -> game_loop ())
;;
