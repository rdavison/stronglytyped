open! Import

module App = struct
  module Model = struct
    type t =
      { keyboard : char Int.Map.t
      ; best : string Int.Map.t
      }
    [@@deriving sexp, equal, fields]

    let cutoff = equal
  end

  module Action = struct
    type t =
      | Swap of int * int
      | Replace of string
      | Store_best of string
    [@@deriving sexp]
  end

  module State = struct
    type t = unit
  end

  let initial_model_exn =
    let mtgap = "ypoujkdlcwinea,mhtsrqz'.;bfvgx" in
    let keyboard =
      mtgap |> String.to_list |> List.mapi ~f:(fun i c -> i, c) |> Int.Map.of_alist_exn
    in
    let best = Int.Map.of_alist_exn [ 0, mtgap ] in
    { Model.keyboard; best }
  ;;

  let on_startup ~schedule_action _model =
    let%map.Deferred () =
      let%map.Deferred corpus =
        Http.get "/static/corpus/typeracer" |> Deferred.Or_error.ok_exn
      in
      Analyzer.Incr.Var.set Analyzer.Corpus.data_v corpus
    in
    Analyzer.Incr.Observer.on_update_exn
      (Analyzer.Incr.observe Analyzer.Root.layout)
      ~f:(function
        | Initialized layout | Changed (_, layout) ->
          schedule_action (Action.Replace layout)
        | Invalidated -> ());
    let on_bestk (analysis : Analyzer.Analysis.t) =
      schedule_action (Store_best analysis.layout)
    in
    let on_swap (a, b) = schedule_action (Swap (a, b)) in
    don't_wait_for (Generator.Cjalgorithm.start 3 ~on_bestk ~on_swap)
  ;;

  let apply_action model =
    let open Incr_dom.Incr.Let_syntax in
    let%map model = model in
    fun (action : Action.t) _ ~schedule_action ->
      let keyboard = Model.keyboard model in
      match action with
      | Action.Swap (ka, kb) ->
        let va = Map.find_exn keyboard ka in
        let vb = Map.find_exn keyboard kb in
        let keyboard = Map.update keyboard ka ~f:(fun _ -> vb) in
        { model with keyboard = Map.update keyboard kb ~f:(fun _ -> va) }
      | Replace s ->
        { model with
          keyboard =
            Int.Map.of_alist_exn (List.init (String.length s) ~f:(fun i -> i, s.[i]))
        }
      | Store_best s ->
        { model with
          best = Map.add_exn (Model.best model) ~key:(Map.length model.best) ~data:s
        }
  ;;

  let view model ~inject =
    let open Incr_dom.Incr.Let_syntax in
    let open Incr_dom.Vdom in
    let keyboard = Incr_dom.Incr.map model ~f:Model.keyboard in
    let%map table =
      let%map top =
        Incr_map.filter_mapi keyboard ~f:(fun ~key:i ~data:c ->
            if i < 10 then Some c else None)
        |> Incr_map.map ~f:(fun c -> Node.td [ Node.text (Char.to_string c) ])
        |> Incr_dom.Incr.map ~f:(fun r -> Map.data r |> Node.tr)
      and middle =
        Incr_map.filter_mapi keyboard ~f:(fun ~key:i ~data:c ->
            if i >= 10 && i < 20 then Some c else None)
        |> Incr_map.map ~f:(fun c -> Node.td [ Node.text (Char.to_string c) ])
        |> Incr_dom.Incr.map ~f:(fun r -> Map.data r |> Node.tr)
      and bottom =
        Incr_map.filter_mapi keyboard ~f:(fun ~key:i ~data:c ->
            if i >= 20 && i < 30 then Some c else None)
        |> Incr_map.map ~f:(fun c -> Node.td [ Node.text (Char.to_string c) ])
        |> Incr_dom.Incr.map ~f:(fun r -> Map.data r |> Node.tr)
      in
      Node.table [ top; middle; bottom ]
    and best =
      let%map best = model >>| Model.best in
      Map.to_alist best ~key_order:`Decreasing
      |> List.map ~f:(fun (_, s) ->
             Node.li
               ~attr:(Attr.on_click (fun _ev -> inject (Action.Replace s)))
               [ Node.text s ])
    in
    Node.body [ Node.h3 [ Node.text "Strongly Typed 💪" ]; table; Node.ul best ]
  ;;

  let create model ~old_model:_ ~inject =
    let open Incr_dom.Incr.Let_syntax in
    let%map apply_action = apply_action model
    and view = view model ~inject
    and model = model in
    Incr_dom.Component.create ~apply_action model view
  ;;
end

let () =
  Incr_dom.Start_app.start
    (module App)
    ~initial_model:App.initial_model_exn
    ~bind_to_element_with_id:"app"
;;
