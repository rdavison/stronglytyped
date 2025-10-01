open! Import

let descending ~size ~compare graph =
  let rec insert lst (weight, item) =
    match lst with
    | [] -> Some [ weight, item ]
    | (biggest_weight, biggest_item) :: rest ->
      let cmp = compare weight biggest_weight in
      if cmp > 0
      then Some ((weight, item) :: (biggest_weight, biggest_item) :: rest)
      else if cmp < 0
      then
        insert rest (weight, item)
        |> Option.map ~f:(fun rest -> (biggest_weight, biggest_item) :: rest)
      else None
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
              (match insert window (weight, item) with
               | None -> n, window
               | Some window ->
                 if n + 1 <= size
                 then n + 1, window
                 else n, List.drop window (n + 1 - size)))
          size
          graph)
      graph
  in
  let window = Bonsai.map n_window ~f:snd in
  window, inject, reset
;;
