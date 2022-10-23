(** This algorithm is an OCaml adaptation and simplification of:
    https://github.com/michaeldickens/Typing/blob/b4431cb690a8df791e5dcae346fdf6955873bdde/cjalgorithm.c 
    
    The algorithm was originally written by Michael Dickens and Chris Johnson.
*)

open! Import

let smart_mutate ~monograms_arr ~swaps ~reverse_lookup_table ~on_swap ~keyboard =
  let monograms_len = Array.length monograms_arr in
  let q = monograms_len / 4 in
  let swaps_len = 2 * swaps in
  let chars_to_swap =
    Array.init swaps_len ~f:(fun _ ->
        let rec loop i default =
          match i < monograms_len with
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
      if i < swaps_len
      then lockins
      else (
        let lc1 = Map.find_exn reverse_lookup_table chars_to_swap.(i) in
        let lc2 = Map.find_exn reverse_lookup_table chars_to_swap.(i + 1) in
        let lockins = (lc1, lc2) :: lockins in
        Root.swap lc1 lc2 ~on_swap;
        loop (i + 2) lockins)
    in
    loop 0 []
  in
  keyboard.Keyboard.next (), Array.of_list_rev lockins
;;

let build_shuffled_indices length =
  let indices = Array.init length ~f:(Fn.const 0) in
  for i = 0 to length - 1 do
    let j = Random.int Int.max_value mod (i + 1) in
    indices.(i) <- indices.(j);
    indices.(j) <- i
  done;
  indices
;;

let improve_layout (score_to_beat : float) lockins ~on_swap ~keyboard =
  let ksize = Vars.Default.ksize in
  let indices = build_shuffled_indices ksize in
  let rec loop_i i =
    let rec loop_j j =
      match j < ksize with
      | false -> return `Continue
      | true ->
        let skip_round =
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
        (match skip_round with
        | true -> loop_j (j + 1)
        | false ->
          let%bind (k : Analysis.t) =
            Root.swap indices.(i) indices.(j) ~on_swap;
            keyboard.Keyboard.next ()
          in
          let score = k.score in
          if Float.(score < score_to_beat)
          then return (`Stop k)
          else (
            (* Undo swap*)
            Root.swap indices.(i) indices.(j) ~on_swap;
            loop_j (j + 1)))
    in
    match i < ksize with
    | false -> return None
    | true ->
      (match%bind loop_j (i + 1) with
      | `Stop x -> return (Some x)
      | `Continue -> loop_i (i + 1))
  in
  loop_i 0
;;

let anneal (k : Analysis.t) lockins ~on_swap ~keyboard =
  let rec loop (last_score : Analysis.t) (score : Analysis.t) =
    let last_improvement =
      if Float.(score.score < last_score.score)
      then last_score.score -. score.score
      else 0.
    in
    let last_score = score in
    let score_to_beat = last_score.score +. last_improvement in
    let%bind score' = improve_layout score_to_beat lockins ~on_swap ~keyboard in
    match score' with
    | None -> return score
    | Some score' ->
      if Float.(score'.score < score_to_beat)
      then loop last_score score'
      else return score
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
    ~chance_to_use_prev
    ~swaps
    ~start_time
    ~num_rounds
    ~bestk
    ~monograms_arr
    ~reverse_lookup_table
    ~on_bestk
    ~on_swap
    ~keyboard
  =
  let rec loop i (prevk, bestk) =
    match i < num_rounds with
    | false -> return bestk
    | true ->
      let%bind prevk' =
        if i > 0 && Float.(Random.float 1. < chance_to_use_prev)
        then
          fst
            (smart_mutate ~monograms_arr ~swaps ~reverse_lookup_table ~on_swap ~keyboard)
        else keyboard.Keyboard.nil ()
      in
      let%bind k = anneal prevk [||] ~on_swap ~keyboard in
      let bestk' =
        if Float.(k.score < bestk.Analysis.score)
        then (
          on_bestk k;
          print_percentages k;
          print_time start_time;
          k)
        else bestk
      in
      loop (i + 1) (prevk', bestk')
  in
  let%bind nil = keyboard.Keyboard.nil () in
  let%map bestk' = loop 0 (nil, bestk) in
  if Float.(bestk'.score < bestk.score) then bestk' else bestk
;;

let reset ?(layout = `Name "dvorak") ?on_swap () =
  Layout.set layout;
  Root.scramble 30 ?on_swap;
  Control.reset ();
  Incr.Var.set Mode.var Ready
;;

(* TODO: Actually support user input. *)
let get_user_input () =
  let start_time = Time.now () in
  let print_time_interval = Vars.Default.print_time_interval in
  let time_on_print =
    Time.Span.( + ) (start_time |> Time.to_span_since_epoch) print_time_interval
  in
  (* Fake user input *)
  Incr.Var.set
    Mode.var
    (Run
       { n = 1
       ; bestk = None
       ; prev_best_fitness = Vars.Default.fitness_max
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
    ~on_bestk
  =
  let is_better = Float.(bestk.Analysis.score < prev_best_fitness) in
  let ts = Time.now () |> Time.to_span_since_epoch in
  let time_on_print' = Time.Span.( + ) ts print_time_interval in
  if is_better
  then (
    let prev_best_fitness = bestk.score in
    on_bestk bestk;
    print_percentages bestk;
    print_time start_time;
    (* If a keyboard was just printed, don't print the time for a while. *)
    prev_best_fitness, time_on_print', print_time_interval)
  else if Time.Span.(ts >= time_on_print) && Vars.Default.detailed_output
  then (
    print_time start_time;
    let print_time_interval =
      (Time.Span.to_sec print_time_interval *. 1.5) +. 1. |> Time.Span.of_sec
    in
    prev_best_fitness, time_on_print', print_time_interval)
  else prev_best_fitness, time_on_print, print_time_interval
;;

let great_to_best
    (bestk : Analysis.t)
    ~num_rounds
    ~monograms_arr
    ~reverse_lookup_table
    ~on_swap
    ~keyboard
  =
  let rec loop i ~(bestk : Analysis.t) ~swaps =
    match i < num_rounds with
    | false -> return bestk
    | true ->
      let swaps =
        if i mod Vars.Default.gtb_rounds_before_swaps_increase
           = Vars.Default.gtb_rounds_before_swaps_increase - 1
        then swaps + 1
        else swaps
      in
      (* Any swaps made by smartMutate() are "locked in" and may not be undone by anneal() *)
      let mutated, lockins =
        smart_mutate ~swaps ~monograms_arr ~reverse_lookup_table ~on_swap ~keyboard
      in
      let%bind mutated = mutated in
      let%bind (k : Analysis.t) =
        (* Use lockins only half the time *)
        if i mod 2 = 0
        then anneal mutated lockins ~on_swap ~keyboard
        else anneal mutated [||] ~on_swap ~keyboard
      in
      let bestk = if Float.(k.score < bestk.score) then k else bestk in
      loop (i + 1) ~bestk ~swaps
  in
  let%map (bestk' : Analysis.t) = loop 0 ~bestk ~swaps:Vars.Default.gtb_number_of_swaps in
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
    ~on_bestk
    ~on_swap
    ~keyboard
  =
  let%bind bestk =
    smart_mutate_and_anneal
      ~chance_to_use_prev
      ~swaps
      ~start_time
      ~num_rounds
      ~bestk
      ~monograms_arr
      ~reverse_lookup_table
      ~on_bestk
      ~on_swap
      ~keyboard
  in
  let prev_best_fitness, time_on_print, print_time_interval =
    update_miscellaneous
      ~bestk
      ~prev_best_fitness
      ~start_time
      ~print_time_interval
      ~time_on_print
      ~on_bestk
  in
  let best_before_gtb = bestk.score in
  let%map (bestk : Analysis.t) =
    great_to_best
      bestk
      ~num_rounds:gtb_rounds
      ~monograms_arr
      ~reverse_lookup_table
      ~on_swap
      ~keyboard
  in
  let prev_best_fitness =
    if Float.(bestk.score < best_before_gtb)
    then (
      let prev_best_fitness = bestk.score in
      if Vars.Default.detailed_output then printf "\n***Found from greatToBest()***\n";
      on_bestk bestk;
      print_percentages bestk;
      print_time start_time;
      prev_best_fitness)
    else prev_best_fitness
  in
  bestk, prev_best_fitness, time_on_print, print_time_interval
;;

let start ?(on_bestk = Fn.const ()) ?(on_swap = Fn.const ()) max_loops ~keyboard =
  reset ();
  let monograms_arr = Incr.observe Corpus.monograms_arr in
  let reverse_lookup_table = Incr.observe Root.reverse_lookup_table in
  let rec loop i =
    if i = max_loops then Incr.Var.set Mode.var Stop;
    let%bind () = Incr.stabilize () in
    let mode = Incr.Observer.value_exn Mode.observer in
    match mode with
    | Mode.Stop -> Deferred.unit
    | Reset ->
      reset ();
      Incr.Var.set Mode.var Ready;
      loop (i + 1)
    | Ready ->
      get_user_input ();
      loop (i + 1)
    | Run { n; bestk; prev_best_fitness; start_time; print_time_interval; time_on_print }
      ->
      let%bind bestk =
        match bestk with
        | None -> keyboard.Keyboard.nil ()
        | Some bestk -> return bestk
      in
      let control = Incr.Observer.value_exn Control.observer in
      let monograms_arr = Incr.Observer.value_exn monograms_arr in
      let reverse_lookup_table = Incr.Observer.value_exn reverse_lookup_table in
      Incr.Var.set Vars.run_v n;
      let%bind bestk, prev_best_fitness, time_on_print, print_time_interval =
        run
          control
          monograms_arr
          reverse_lookup_table
          ~bestk
          ~prev_best_fitness
          ~start_time
          ~print_time_interval
          ~time_on_print
          ~num_rounds:Vars.Default.algorithm_rounds
          ~on_bestk
          ~on_swap
          ~keyboard
      in
      let mode' =
        match n < Vars.Default.max_runs with
        | false -> Mode.Stop
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
      Incr.Var.set Mode.var mode';
      loop (i + 1)
  in
  loop 1
;;
