open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let brute_force_indexes ~keyboard_inject ~keyboard_cancel ~keyboard graph =
  let%arr keyboard_inject = keyboard_inject
  and keyboard_cancel = keyboard_cancel
  and indexes_swaps_for_brute_forcing =
    let%arr x =
      Bonsai.assoc
        (module Analysis.Hand)
        (Bonsai.Map.index_by
           keyboard
           ~comparator:(module Analysis.Hand)
           ~index:(fun (key : Analysis.Key.t) ->
             if Analysis.Finger.equal key.finger `i then Some key.hand else None)
           graph)
        ~f:(fun _ data graph ->
          let%arr keys = Bonsai.Map.keys data graph in
          let keys = Set.to_list keys in
          let visited = ref Analysis.Key.Id.Pair.Set.empty in
          List.cartesian_product keys keys
          |> List.filter_map ~f:(fun (k1, k2) ->
            if Key.Id.equal k1 k2
            then None
            else (
              let k12 = k1, k2 in
              let k21 = k2, k1 in
              let curr = !visited in
              match Set.mem curr k12, Set.mem curr k21 with
              | false, false ->
                visited := Set.add !visited k12;
                visited := Set.add !visited k21;
                Some k12
              | _, _ -> None)))
        graph
    in
    List.concat (Map.data x)
  in
  let effects =
    List.map indexes_swaps_for_brute_forcing ~f:(fun swap -> Keyboard.Action.Swap swap)
  in
  Ui_effect.all_unit [ keyboard_cancel; keyboard_inject effects ]
;;
