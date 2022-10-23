open! Import

module Model = struct
  type t =
    { analysis : Analyzer.Analysis.t option
    ; best : Analyzer.Analysis.t Int.Map.t
    ; score_box : string
    ; score_button_text : string
    ; vars_to_observe : string list
    ; generator_state : [ `Stopped | `Running ]
    ; stats : float Stats2.t
    ; print_all_stats : bool
    }
  [@@deriving sexp, compare, fields]

  let cutoff = [%compare.equal: t]
end

module Action = struct
  type t =
    | Compile_and_run
    | Analyze of string
    | Stats_and_analysis of float Stats2.t * Analyzer.Analysis.t
    | Store_best of Analyzer.Analysis.t
    | Update_score_box of string
  [@@deriving sexp]
end

module State = struct
  type t = unit
end

let initial_model_exn =
  { Model.analysis = None
  ; best = Int.Map.empty
  ; score_box = ""
  ; score_button_text = ""
  ; vars_to_observe = []
  ; stats = { hf = String.Map.empty; hand = String.Map.empty }
  ; generator_state = `Stopped
  ; print_all_stats = true
  }
;;

let on_startup ~schedule_action _model =
  let%map.Deferred corpus =
    Http.get "/static/corpus/typeracer" |> Deferred.Or_error.ok_exn
  in
  Analyzer.Incr.Var.set Analyzer.Corpus.data_v corpus
;;

let set_error model e =
  print_endline e;
  model
;;

let compile_and_run (model : Model.t) ~schedule_action =
  let syntax = model.score_box in
  match Analyzer.Score.Ast.of_syntax syntax with
  | Error e -> set_error model e
  | Ok ast ->
    (match Analyzer.Score.Ast.compile ast with
    | Error e -> set_error model e
    | Ok incr ->
      let vars =
        if model.Model.print_all_stats
        then Analyzer.Score.Ast.all_vars |> List.map ~f:fst
        else Analyzer.Score.Ast.uniq_vars ast
      in
      let analysis = Analyzer.Analysis.make_incr incr in
      let keyboard = Generator.Keyboard.make analysis in
      let stats_and_analysis =
        let%map.Analyzer.Incr stats = vars |> Analyzer.Score.Ast.stats_of_vars
        and analysis = analysis in
        stats, analysis
      in
      Analyzer.Incr.Observer.on_update_exn
        (Analyzer.Incr.observe stats_and_analysis)
        ~f:(function
          | Initialized ((hf_stats, hand_stats), analysis)
          | Changed (_, ((hf_stats, hand_stats), analysis)) ->
            let stats =
              let hf = String.Map.of_alist_exn hf_stats in
              let hand = String.Map.of_alist_exn hand_stats in
              { Stats2.hf; hand }
            in
            schedule_action (Action.Stats_and_analysis (stats, analysis))
          | Invalidated -> ());
      don't_wait_for
        (Generator.Cjalgorithm.start
           3
           ~on_bestk:(fun best -> schedule_action (Action.Store_best best))
           ~keyboard);
      { model with vars_to_observe = vars })
;;

let apply_action model =
  let open Incr_dom.Incr.Let_syntax in
  let%map (model : Model.t) = model in
  fun (action : Action.t) _ ~schedule_action ->
    match action with
    | Update_score_box s -> { model with score_box = s }
    | Compile_and_run ->
      compile_and_run { model with print_all_stats = false } ~schedule_action
    | Analyze s ->
      print_endline "here";
      Analyzer.Layout.set (`Layout s);
      { model with print_all_stats = true }
    | Action.Stats_and_analysis (stats, analysis) ->
      { model with analysis = Some analysis; stats }
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

let view old_model model ~inject =
  let open Incr_dom.Incr.Let_syntax in
  let open Incr_dom.Vdom in
  let%map keyboard =
    let%bind analysis = model >>| Model.analysis in
    match analysis with
    | None -> Incr_dom.Incr.return (Node.div [ Node.text "<keyboard>" ])
    | Some analysis ->
      Keyboard.view (analysis.layout |> Keyboard.of_string |> Incr_dom.Incr.return)
  and score =
    let open Incr_dom.Vdom in
    let%map model = model in
    let label = Node.label [ Node.text "Score Editor" ] in
    let input =
      Node.input
        ~attr:
          (Attr.many_without_merge
             [ Attr.id "score_editor"
             ; Attr.type_ "text"
             ; Attr.string_property "value" model.Model.score_box
             ; Attr.on_input (fun _ev text -> inject (Action.Update_score_box text))
             ])
        []
    in
    let button =
      Node.button
        ~attr:
          (Attr.many_without_merge
             [ Attr.id (String.lowercase "score_editor_submit_button")
             ; Attr.on_click (fun _ev -> inject Action.Compile_and_run)
             ])
        [ Node.text model.score_button_text ]
    in
    Node.div [ label; input; button ]
  and stats2 =
    let%map model = model
    and old_model = old_model in
    let diff = Stats2.diff_color old_model.Model.stats model.stats in
    Stats2.view diff (fun (cell, color) ->
        let open Incr_dom.Vdom in
        let data = cell |> Float.( * ) 100. |> sprintf "%.2f" |> Node.text in
        let attr = Option.map color ~f:(fun color -> Attr.style (Css_gen.color color)) in
        Node.td ?attr [ data ])
  and best =
    let best = model >>| Model.best in
    Incr_map.map best ~f:(fun (s : Analyzer.Analysis.t) ->
        let%map keyboard =
          s.layout |> Keyboard.of_string |> Incr_dom.Incr.return |> Keyboard.view
        in
        Node.li
          ~attr:(Attr.on_click (fun _ev -> inject (Action.Analyze s.layout)))
          [ keyboard ])
    |> Incr_map.join
    |> Incr_dom.Incr.map ~f:(fun map ->
           map |> Map.to_alist ~key_order:`Decreasing |> List.map ~f:snd)
  in
  Node.body
    [ Node.h3 [ Node.text "Strongly Typed 💪" ]
    ; Node.create
        "article"
        [ Node.div ~attr:(Attr.class_ "kb-wrapper") [ keyboard ]
        ; score
        ; stats2
        ; Node.ul best
        ]
    ]
;;

let create model ~old_model ~inject =
  let open Incr_dom.Incr.Let_syntax in
  let%map apply_action = apply_action model
  and view = view old_model model ~inject
  and model = model in
  Incr_dom.Component.create ~apply_action model view
;;
