open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

module Style =
  [%css
    stylesheet
      {|
        .stats-vertical-analysis {
          padding: 2rem;
        }

        .stats-vertical-analysis table {
          border-collapse: collapse;
          font-family: monospace;
        }

        .stats-vertical-analysis table th {
          font-weight: bold;
          color: black;
          padding: 0.25rem;
          border: 1px black solid;
        }

        .stats-vertical-analysis table td {
          color: black;
          padding: 1rem;
          border: 1px black solid;
          text-align: center;
          align-items: center;
        }

        .stats-vertical-analysis table thead th {
          border-top: 0;
        }

        .stats-vertical-analysis table tbody th {
          border-top: 1;
        }

        .stats-vertical-analysis table tr:last-child th,
        .stats-vertical-analysis table tr:last-child td {
          border-bottom: 0;
        }

        .stats-vertical-analysis table tr th:first-child,
        .stats-vertical-analysis table tr td:first-child {
          border-left: 0;
        }

        .stats-vertical-analysis table tr th:last-child,
        .stats-vertical-analysis table tr td:last-child {
          border-right: 0;
        }

        .stats-vertical-analysis-inner-table table {
          border-radius: 0;
          border-collapse: collapse;
          border: 0;
          margin: 0;
        }

        .stats-vertical-analysis-inner-table table td {
          border: 1px black solid;
          padding: 0.5rem;
          color: black;
        }

        .stats-vertical-analysis-inner-table table tr:first-child td {
          border-top: 0;
        }

        .stats-vertical-analysis-inner-table table tr:last-child td {
          border-bottom: 0;
        }

        .stats-vertical-analysis-inner-table table tr td:first-child {
          border-left: 0;
        }

        .stats-vertical-analysis-inner-table table tr td:last-child {
          border-right: 0;
        }
|}]

let multiselect graph =
  Form.Elements.Multiselect.set
    (module Analysis.Stats_same_finger.Typed_variant.Packed)
    (Bonsai.return Analysis.Stats_same_finger.Typed_variant.Packed.all)
    graph
;;

let selected_metrics form =
  let%arr form = form in
  Form.value_or_default form ~default:[]
;;

let label (t : Analysis.Stats_same_finger.Typed_variant.Packed.t) =
  match t.f with
  | T Sfb -> "SFB"
  | T Sfs -> "SFS"
  | T Speed -> "Speed"
  | T Sfb_worst -> "Worst SFB"
  | T Sfs_worst -> "Worst SFS"
  | T Speed_worst -> "Worst Speed"
;;

let render_speed x = sprintf "%.2fd/t" (Float.abs (x *. 100.))
let render_freq x = sprintf "%.2f%%" (Float.abs (x *. 100.))

let render_measurement (t : Analysis.Stats_same_finger.Typed_variant.Packed.t) =
  match t.f with
  | T Sfb -> render_freq
  | T Sfs -> render_freq
  | T Speed -> render_speed
  | T Sfb_worst -> render_freq
  | T Sfs_worst -> render_freq
  | T Speed_worst -> render_speed
;;

let render_simple
      (t :
        (float, 'total) Analysis.Stats_same_finger.metric
          Analysis.Stats_same_finger.Typed_variant.t)
      data
  =
  let packed = Analysis.Stats_same_finger.Typed_variant.Packed.pack t in
  Vdom.Node.text (render_measurement packed data)
;;

let render_detailed
      (t :
        ((string * float) list, 'total) Analysis.Stats_same_finger.metric
          Analysis.Stats_same_finger.Typed_variant.t)
      worst
  =
  let packed = Analysis.Stats_same_finger.Typed_variant.Packed.pack t in
  Vdom.Node.div
    ~attrs:[ Style.stats_vertical_analysis_inner_table ]
    [ Vdom.Node.table
        (List.map worst ~f:(fun (corpus_key, metric) ->
           Vdom.Node.tr
             [ Vdom.Node.td [ Vdom.Node.text corpus_key ]
             ; Vdom.Node.td [ Vdom.Node.text (render_measurement packed metric) ]
             ]))
    ]
;;

let render_breakdown
  : type breakdown.
    (breakdown, 'total) Analysis.Stats_same_finger.metric
      Analysis.Stats_same_finger.Typed_variant.t
    -> breakdown
    -> Vdom.Node.t
  =
  fun t breakdown ->
  match t with
  | Sfb -> render_simple Sfb breakdown
  | Sfs -> render_simple Sfs breakdown
  | Speed -> render_simple Speed breakdown
  | Sfb_worst -> render_detailed Sfb_worst breakdown
  | Sfs_worst -> render_detailed Sfs_worst breakdown
  | Speed_worst -> render_detailed Speed_worst breakdown
;;

let render_total t total = Vdom.Node.text (render_measurement t total)

let row
      (t :
        ('breakdown, 'total) Analysis.Stats_same_finger.metric
          Analysis.Stats_same_finger.Typed_variant.t)
      ~(metric : ('breakdown, 'total) Analysis.Stats_same_finger.metric Bonsai.t)
      graph
  : Vdom.Node.t Bonsai.t
  =
  let packed = Analysis.Stats_same_finger.Typed_variant.Packed.pack t in
  let%sub { breakdown; total } = metric in
  let breakdown =
    Bonsai.assoc
      (module Analysis.Hand_finger)
      breakdown
      ~f:(fun _key data _graph ->
        let%arr contents = Bonsai.map data ~f:(render_breakdown t) in
        Vdom.Node.td [ contents ])
      graph
  in
  let total = Bonsai.map total ~f:(render_total packed) in
  let label = label packed in
  let%arr breakdown = breakdown
  and total = total in
  let header = Vdom.Node.th [ Vdom.Node.text label ] in
  let breakdown =
    List.map Analysis.Hand_finger.all ~f:(fun hand_finger ->
      Map.find breakdown hand_finger
      |> Option.value ~default:(Vdom.Node.td [ Vdom.Node.none ]))
  in
  let total = Vdom.Node.td [ total ] in
  Vdom.Node.tr ((header :: breakdown) @ [ total ])
;;

let table
      (t :
        ( Analysis.Stats_same_finger.Typed_variant.Packed.t
          , Analysis.Stats_same_finger.t
          , Analysis.Stats_same_finger.Typed_variant.Packed.comparator_witness )
          Map_intf.Map.t
          Bonsai.t)
      metrics_order
      graph
  =
  let data =
    Bonsai.assoc
      (module Analysis.Stats_same_finger.Typed_variant.Packed)
      t
      ~f:(fun _key data graph ->
        match%sub data with
        | Sfb metric -> row Sfb ~metric graph
        | Sfs metric -> row Sfs ~metric graph
        | Speed metric -> row Speed ~metric graph
        | Sfb_worst metric -> row Sfb_worst ~metric graph
        | Sfs_worst metric -> row Sfs_worst ~metric graph
        | Speed_worst metric -> row Speed_worst ~metric graph)
      graph
  in
  let%arr data = data in
  let header =
    let all = Analysis.Hand_finger.all in
    let to_string = Analysis.Hand_finger.to_string in
    (all |> List.map ~f:to_string) @ [ "Total" ]
  in
  let scope x = Vdom.Attr.create "scope" x in
  Vdom.Node.table
    ~attrs:[]
    [ Vdom.Node.thead
        [ Vdom.Node.tr
            (Vdom.Node.th ~attrs:[] [ (* empty corner cell *) ]
             :: List.map header ~f:(fun label ->
               Vdom.Node.th ~attrs:[ scope "col" ] [ Vdom.Node.text label ]))
        ]
    ; Vdom.Node.tbody
        (List.map metrics_order ~f:(fun metric -> Map.find data metric) |> List.filter_opt)
    ]
;;

let component ~keyboard ~corpus ~worst_counter graph =
  let controls = multiselect graph in
  let metrics =
    let%arr controls = controls in
    Form.value_or_default
      controls
      ~default:
        (Set.of_list
           (module Analysis.Stats_same_finger.Typed_variant.Packed)
           Analysis.Stats_same_finger.Typed_variant.Packed.all)
  in
  let diff_row_bigram_data =
    let bigram_data = Analysis.Bigram_data.make keyboard corpus graph in
    Analysis.Stats_same_finger.bigram_data bigram_data graph
  in
  let stats_same_finger =
    Analysis.Stats_same_finger.component
      ~metrics
      ~worst_counter
      ~diff_row_bigram_data
      graph
  in
  let table =
    table stats_same_finger Analysis.Stats_same_finger.Typed_variant.Packed.all graph
  in
  let vdom =
    let%arr table = table in
    Vdom.Node.div ~attrs:[ Style.stats_vertical_analysis ] [ table ]
  in
  controls, vdom
;;
