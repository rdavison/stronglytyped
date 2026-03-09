open! Import

let score
      ~(same_finger_stats :
         ( Stats_same_finger.Typed_variant.Packed.t
           , Stats_same_finger.t
           , Stats_same_finger.Typed_variant.Packed.comparator_witness )
           Map.t
           Bonsai.t)
  =
  let%arr same_finger_stats = same_finger_stats in
  let speed =
    Map.find same_finger_stats (Stats_same_finger.Typed_variant.Packed.pack Speed)
  in
  Option.bind speed ~f:(function
    | Sfb _ | Sfs _ | Sfb_worst _ | Sfs_worst _ | Speed_worst _ -> None
    | Speed speed -> Some (speed.total *. 100.))
;;

let greedy_descend ~input_keyboard ~live_keyboard_inject ~corpus graph =
  let acc_keyboard, acc_keyboard_inject, _clear = Keyboard.state_machine graph in
  Bonsai.Edge.on_change
    ~equal:Keyboard.equal
    acc_keyboard
    ~callback:
      (let%arr live_keyboard_inject = live_keyboard_inject in
       fun acc_keyboard ->
         live_keyboard_inject
           [ Keyboard.Action.Overwrite
               (Map.map acc_keyboard ~f:(fun (key : Key.t) -> key.kc))
           ])
    graph;
  let metrics =
    Core.Set.of_list
      (module Stats_same_finger.Typed_variant.Packed)
      Stats_same_finger.Typed_variant.Packed.all
  in
  let diff_row_bigram_data =
    let data = Bigram_data.make acc_keyboard corpus graph in
    Stats_same_finger.bigram_data data graph
  in
  let same_finger_stats =
    Stats_same_finger.component
      ~metrics:(Bonsai.return metrics)
      ~worst_counter:(Bonsai.return 0)
      ~diff_row_bigram_data
      graph
  in
  let keyboard_score =
    let%arr acc_keyboard, same_finger_stats = same_finger_stats in
    let res =
      let speed =
        Map.find same_finger_stats (Stats_same_finger.Typed_variant.Packed.pack Speed)
      in
      Option.bind speed ~f:(function
        | Sfb _ | Sfs _ | Sfb_worst _ | Sfs_worst _ | Speed_worst _ -> None
        | Speed speed -> Some (speed.total *. 100.))
    in
    acc_keyboard, res
  in
  let minimum, set_minimum = Bonsai.state' None graph in
  let track =
    let%arr set_minimum = set_minimum in
    fun (score, keyboard) ->
      let default = Option.map score ~f:(fun score -> score, keyboard) in
      set_minimum (fun prev ->
        let res =
          match prev with
          | None -> default
          | Some ((prev_score, prev_keyboard) as prev) ->
            (match default with
             | None -> Some prev
             | Some (score, keyboard) ->
               (* printf *)
               (*   "track: prev: %.4f score: %.4f keyboard: %s\n%!" *)
               (*   prev_score *)
               (*   score *)
               (*   (Keyboard.to_string keyboard); *)
               if
                 [%compare: float * Keyboard.t]
                   (prev_score, prev_keyboard)
                   (score, keyboard)
                 < 0
               then Some (prev_score, prev_keyboard)
               else Some (score, keyboard))
        in
        res)
  in
  (* let inject : (Keyboard.t -> Key.Id.Pair.t -> unit Ui_effect.t) Bonsai.t = *)
  (*   let%arr track = track *)
  (*   and keyboard, score = keyboard_score *)
  (*   and keyboard_inject = keyboard_inject in *)
  (*   fun keeb pair -> *)
  (*     let swap = Keyboard.swap keeb pair in *)
  (*     let%bind.Ui_effect () = track (score, keyboard) in *)
  (*     match swap with *)
  (*     | None -> Ui_effect.Ignore *)
  (*     | Some swapped -> *)
  (*       let overwrite = Key.Id.Map.map swapped ~f:(fun (key : Key.t) -> key.kc) in *)
  (*       keyboard_inject [ Overwrite overwrite ] *)
  (* in *)
  let () =
    Bonsai.Edge.on_change
      ~equal:[%equal: Keyboard.t * float option]
      keyboard_score
      ~callback:
        (let%arr track = track in
         fun (keyboard, score) -> track (score, keyboard))
      graph
  in
  let () =
    Bonsai.Edge.on_change
      ~equal:[%equal: Keyboard.t Key.Id.Pair.Map.t]
      (Keyboard.all_swaps input_keyboard graph)
      ~callback:
        (let%arr acc_keyboard_inject = acc_keyboard_inject in
         fun all_swaps ->
           all_swaps
           |> Map.data
           |> List.map ~f:(fun swap ->
             Keyboard.Action.Overwrite
               (swap |> Key.Id.Map.map ~f:(fun (key : Key.t) -> key.kc)))
           |> acc_keyboard_inject)
      graph
  in
  minimum
;;
