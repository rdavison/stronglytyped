open! Import

module App = struct
  module Model = struct
    type t =
      { analysis : Analyzer.Analysis.t option
      ; best : Analyzer.Analysis.t Int.Map.t
      }
    [@@deriving sexp, compare, fields]

    let cutoff = [%compare.equal: t]
  end

  module Action = struct
    type t =
      | Analysis of Analyzer.Analysis.t
      | Store_best of Analyzer.Analysis.t
    [@@deriving sexp]
  end

  module State = struct
    type t = unit
  end

  let initial_model_exn = { Model.analysis = None; best = Int.Map.empty }

  let on_startup ~schedule_action _model =
    let%map.Deferred () =
      let%map.Deferred corpus =
        Http.get "/static/corpus/typeracer" |> Deferred.Or_error.ok_exn
      in
      Analyzer.Incr.Var.set Analyzer.Corpus.data_v corpus
    in
    Analyzer.Incr.Observer.on_update_exn
      (Analyzer.Incr.observe Analyzer.Analysis.incr)
      ~f:(function
        | Initialized analysis | Changed (_, analysis) ->
          schedule_action (Action.Analysis analysis)
        | Invalidated -> ());
    let on_bestk (analysis : Analyzer.Analysis.t) =
      schedule_action (Store_best analysis)
    in
    don't_wait_for (Generator.Cjalgorithm.start 3 ~on_bestk)
  ;;

  let apply_action model =
    let open Incr_dom.Incr.Let_syntax in
    let%map (model : Model.t) = model in
    fun (action : Action.t) _ ~schedule_action ->
      match action with
      | Action.Analysis analysis -> { model with analysis = Some analysis }
      | Store_best analysis ->
        let last = Map.find model.best (Map.length model.best - 1) in
        (match last with
        | None ->
          let best =
            Map.add_exn (Model.best model) ~key:(Map.length model.best) ~data:analysis
          in
          { model with best }
        | Some last ->
          (match String.equal last.layout analysis.layout with
          | true -> model
          | false ->
            let best =
              Map.add_exn (Model.best model) ~key:(Map.length model.best) ~data:analysis
            in
            { model with best }))
  ;;

  let view model ~inject =
    let open Incr_dom.Incr.Let_syntax in
    let open Incr_dom.Vdom in
    let%map table =
      let%bind analysis = model >>| Model.analysis in
      match analysis with
      | None -> Incr_dom.Incr.return (Node.text "<keyboard>")
      | Some analysis ->
        Keyboard.view (analysis.layout |> Keyboard.of_string |> Incr_dom.Incr.return)
    and stats =
      let%map analysis = model >>| Model.analysis in
      let stats =
        Option.bind analysis ~f:(fun (analysis : Analyzer.Analysis.t) -> analysis.stats)
      in
      match stats with
      | None -> Node.text "<stats>"
      | Some stats -> Stats.view stats
    and best =
      let best = model >>| Model.best in
      Incr_map.map best ~f:(fun (s : Analyzer.Analysis.t) ->
          let%map keyboard =
            s.layout |> Keyboard.of_string |> Incr_dom.Incr.return |> Keyboard.view
          in
          Node.li
            ~attr:(Attr.on_click (fun _ev -> inject (Action.Analysis s)))
            [ keyboard ])
      |> Incr_map.join
      |> Incr_dom.Incr.map ~f:(fun map ->
             map |> Map.to_alist ~key_order:`Decreasing |> List.map ~f:snd)
    in
    Node.body [ Node.h3 [ Node.text "Strongly Typed 💪" ]; table; stats; Node.ul best ]
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
