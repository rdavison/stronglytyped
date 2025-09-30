open! Import

let descending ~size ~compare graph =
  let rec insert lst (weight, item) =
    match lst with
    | [] -> [ weight, item ]
    | (biggest_weight, biggest_item) :: rest ->
      if compare weight biggest_weight >= 0
      then (weight, item) :: (biggest_weight, biggest_item) :: rest
      else (biggest_weight, biggest_item) :: insert rest (weight, item)
  in
  let default_model = 0, [] in
  let (n_window, inject), reset =
    Bonsai.with_model_resetter_n
      ~n:Autopack.Two
      ~f:(fun graph ->
        Bonsai.state_machine_with_input
          ~reset:(fun _ctx _ -> default_model)
          ~default_model
          ~apply_action:(fun _ctx input (n, window) (weight, item) ->
            match input with
            | Inactive -> n, window
            | Active size ->
              let window = insert window (weight, item) in
              if n + 1 <= size then n + 1, window else n, List.drop window (n + 1 - size))
          size
          graph)
      graph
  in
  let window = Bonsai.map n_window ~f:snd in
  window, inject, reset
;;
