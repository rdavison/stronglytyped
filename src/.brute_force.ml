open! Import

type t =
  { score : float
  ; pretty : string
  ; save_state : Layout.save_state
  }

let run (layout : Layout.t) ~corpus ~weights =
  let stats = Stats.make layout corpus in
  let score = Score.make stats ~weights in
  let observer = Incr.observe score in
  Incr.stabilize ();
  let best_save_state = ref (Layout.save layout) in
  let best_score = ref (Incr.Observer.value_exn observer) in
  let iteration = ref 0 in
  printf "Generating list of valid swaps\n%!";
  let valid_swaps = Layout.swaps layout in
  printf "Valid swaps: %d\n%!" (Array.length valid_swaps);
  let continue_ = ref true in
  while !continue_ do
    continue_ := false;
    incr iteration;
    printf "Brute Force Round %d...\n%!" !iteration;
    let save_states =
      Array.fold valid_swaps ~init:[] ~f:(fun save_states (s1, s2) ->
        let a, b = s1 in
        let c, d = s2 in
        Layout.swap layout a b;
        Layout.swap layout c d;
        let save_state = Layout.save layout in
        Layout.swap layout c d;
        Layout.swap layout a b;
        save_state :: save_states)
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
