open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view
include Analysis.Stats_same_finger

let bar ~dir ~color ~pct =
  let flex_direction =
    match dir with
    | `H -> "row"
    | `V -> "column-reverse"
  in
  let flex_bar = pct |> Float.to_string in
  let flex_blank = 1. -. pct |> Float.to_string in
  let bar =
    Vdom.Node.create
      "bar-fill"
      ~attrs:
        [ [%css
            {|
              flex: %{flex_bar};
              background: %{color};
            |}]
        ]
      []
  in
  let blank =
    Vdom.Node.create
      "bar-blank"
      ~attrs:
        [ [%css
            {|
              flex: %{flex_blank};
            |}]
        ]
      []
  in
  Vdom.Node.create
    "bar"
    ~attrs:
      [ [%css
          {|
            display: flex;
            width: 100%;
            height: 100%;
            flex-direction: %{flex_direction};
          |}]
      ]
    [ bar; blank ]
;;

(* let bar_attr height = *)
(*   [%css *)
(*     {| *)
(*       flex: %{sprintf "%f" height}; *)
(*     |}] *)
(* ;; *)

let multiselect graph =
  let%arr form =
    Form.Elements.Multiselect.set
      ~default_selection_status:
        (Bonsai.return Bonsai_web_ui_multi_select.Selection_status.Selected)
      (module Typed_variant.Packed)
      (Bonsai.return Typed_variant.Packed.all)
      graph
  in
  Form.map_view form ~f:(fun view ->
    Vdom.Node.div
      ~attrs:
        [ [%css
            {|
              .multi-select-container {
                font-family: monospace;
              }
            |}]
        ]
      [ view ])
;;

let selected_metrics form =
  let%arr form = form in
  Form.value_or_default form ~default:[]
;;

let label (t : Typed_variant.Packed.t) =
  match t.f with
  | T Sfb -> "SFB"
  | T Sfs -> "SFS"
  | T Speed -> "Speed"
  | T Sfb_worst -> "Worst SFB"
  | T Sfs_worst -> "Worst SFS"
  | T Speed_worst -> "Worst Speed"
;;

let render_speed x = Vdom.Node.textf "%.2fd/t" (Float.abs (x *. 100.))
let render_freq x = Vdom.Node.textf "%.2f%%" (Float.abs (x *. 100.))

let render_measurement (t : Typed_variant.Packed.t) =
  match t.f with
  | T Sfb -> render_freq
  | T Sfs -> render_freq
  | T Speed -> render_speed
  | T Sfb_worst -> render_freq
  | T Sfs_worst -> render_freq
  | T Speed_worst -> render_speed
;;

let _render_simple (t : (float, 'total) metric Typed_variant.t) data =
  let packed = Typed_variant.Packed.pack t in
  render_measurement packed data
;;

let css_attr_target n target curr =
  let scale x =
    let open Float in
    5. *. exp (-1. *. n *. exp 1. *. (x ** 2.)) /. (2. *. sqrt (2. *. pi))
  in
  let a = Float.to_string (scale (Float.abs (target -. curr))) in
  [%string "hsl(calc(0 + (142 - 0) * %{a}),70%,45%)"]
;;

let render_simple_with_target
      n
      (_t : (float, float) metric Typed_variant.t)
      target
      breakdown
      total
  =
  (* let packed = Typed_variant.Packed.pack t in *)
  let scale x = Float.sqrt (1. -. ((x -. 1.) ** 2.)) in
  let height = scale (breakdown /. total) in
  bar ~dir:`V ~color:(css_attr_target n target breakdown) ~pct:height
;;

let render_total2 n (packed : Typed_variant.Packed.t) target total =
  let scale x =
    match packed.f with
    | T Sfb | T Sfs | T Sfb_worst | T Sfs_worst -> Float.sqrt (1. -. ((x -. 1.) ** 2.))
    | T Speed | T Speed_worst -> total
  in
  let height = scale total in
  bar ~dir:`V ~color:(css_attr_target n target total) ~pct:height
;;

(* let css_attr_prev_curr n _good_dir target _prev curr = css_attr_target n target curr *)

(* let _render_prev_curr *)
(*       n *)
(*       (t : (float option * float, 'total) metric Typed_variant.t) *)
(*       good_dir *)
(*       target *)
(*       ((prev, curr) : float option * float) *)
(*   = *)
(*   let packed = Typed_variant.Packed.pack t in *)
(*   Vdom.Node.div *)
(*     ~attrs:[ css_attr_prev_curr n good_dir target prev curr ] *)
(*     [ render_measurement packed curr ] *)
(* ;; *)

(* let table_color = Tailwind_v3_colors.slate900 *)

let grid_attr ?(padding = "1rem") cols =
  let cols = Int.to_string cols in
  [%css
    {|
      display: grid;
      grid-template-columns: repeat(%{cols}, 1fr);
      gap: 1px;
      background: %{Design.(Card.border_color theme)#Css_gen.Color};
      overflow: hidden;
      width: 100%;

      & > header {
        background: %{Design.(Card.background_color theme)#Css_gen.Color};
        font-weight: bold;
        padding: %{padding};
        margin: ${padding};
        text-align: center;
        font-family: monospace;
      }

      & > cell, & > total {
        background: %{Design.(Card.background_color theme)#Css_gen.Color};
        padding: 0;
        text-align: center;
        font-family: monospace;
      }
    |}]
;;

let render_detailed
      (_t : ((string * float) list * float, float) metric Typed_variant.t)
      (worst, per_finger_total)
      _total
  =
  (* let packed = Typed_variant.Packed.pack t in *)
  Vdom.Node.div
    ~attrs:[ Design.Card.attr; [%css {|border: unset;|}] ]
    [ Vdom.Node.div
        ~attrs:[ grid_attr ~padding:"0.5rem" 2 ]
        (List.concat_map worst ~f:(fun (corpus_key, breakdown) ->
           let height = breakdown /. per_finger_total in
           [ Vdom.Node.header [ Vdom.Node.text corpus_key ]
           ; Vdom.Node.create
               "cell"
               ~attrs:[ [%css {|padding: unset;|}] ]
               [ bar ~dir:`H ~color:"tomato" ~pct:height ]
           ]))
    ]
;;

let render_breakdown_1
  : float -> (float, float) metric Typed_variant.t -> float -> float -> Vdom.Node.t
  =
  fun n t breakdown total ->
  match t with
  | Sfb -> render_simple_with_target n Sfb 0. breakdown total
  | Sfs -> render_simple_with_target n Sfs 0. breakdown total
  | Speed -> render_simple_with_target n Speed 0. breakdown total
;;

let render_breakdown_2
  :  float
  -> ((string * float) list * float, float) metric Typed_variant.t
  -> (string * float) list * float
  -> float
  -> Vdom.Node.t
  =
  fun _n t breakdown total ->
  match t with
  | Sfb_worst -> render_detailed Sfb_worst breakdown total
  | Sfs_worst -> render_detailed Sfs_worst breakdown total
  | Speed_worst -> render_detailed Speed_worst breakdown total
;;

let render_total
  : type breakdown total.
    float -> (breakdown, float) metric Typed_variant.t -> float -> Vdom.Node.t
  =
  fun n t total ->
  let packed = Typed_variant.Packed.pack t in
  render_total2 n packed 0. total
;;

let row_1
      (dexterity : (Analysis.Hand_finger.t -> float) Bonsai.t)
      (variant : (float, float) metric Typed_variant.t)
      ~(metric : (float, float) metric Bonsai.t)
      graph
  : Vdom.Node.t list Bonsai.t
  =
  let packed = Typed_variant.Packed.pack variant in
  let%sub { breakdown; total } = metric in
  let breakdown =
    Bonsai.assoc
      (module Analysis.Hand_finger)
      breakdown
      ~f:(fun hand_finger breakdown _graph ->
        let contents =
          let open Bonsai.Applicative_infix in
          let%arr dexterity = dexterity <*> hand_finger
          and breakdown = breakdown
          and total = total in
          match variant with
          | Sfb -> render_breakdown_1 dexterity Sfb breakdown total
          | Sfs -> render_breakdown_1 dexterity Sfs breakdown total
          | Speed -> render_breakdown_1 dexterity Speed breakdown total
        in
        let%arr contents = contents in
        Vdom.Node.create
          "cell"
          ~attrs:
            [ [%css
                {|
                  display: flex;
                  justify-content: center;
                |}]
            ]
          [ contents ])
      graph
  in
  let total = Bonsai.map total ~f:(render_total 1. variant) in
  let label = label packed in
  let%arr breakdown = breakdown
  and total = total in
  let header =
    Vdom.Node.header ~attrs:[ [%css {|font-weight: bold;|}] ] [ Vdom.Node.text label ]
  in
  let breakdown =
    List.map Analysis.Hand_finger.all ~f:(fun hand_finger ->
      Map.find breakdown hand_finger
      |> Option.value ~default:(Vdom.Node.div [ Vdom.Node.none ]))
  in
  let total = Vdom.Node.create "total" [ total ] in
  (header :: breakdown) @ [ total ]
;;

let row_2
      (dexterity : (Analysis.Hand_finger.t -> float) Bonsai.t)
      (variant : ((string * float) list * float, float) metric Typed_variant.t)
      ~(metric : ((string * float) list * float, float) metric Bonsai.t)
      graph
  : Vdom.Node.t list Bonsai.t
  =
  let packed = Typed_variant.Packed.pack variant in
  let%sub { breakdown; total } = metric in
  let breakdown =
    Bonsai.assoc
      (module Analysis.Hand_finger)
      breakdown
      ~f:(fun hand_finger breakdown _graph ->
        let contents =
          let open Bonsai.Applicative_infix in
          let%arr dexterity = dexterity <*> hand_finger
          and breakdown = breakdown
          and total = total in
          match variant with
          | Sfb_worst -> render_breakdown_2 dexterity Sfb_worst breakdown total
          | Sfs_worst -> render_breakdown_2 dexterity Sfs_worst breakdown total
          | Speed_worst -> render_breakdown_2 dexterity Speed_worst breakdown total
        in
        let%arr contents = contents in
        Vdom.Node.create
          "cell"
          ~attrs:
            [ [%css
                {|
                  display: flex;
                  justify-content: center;
                |}]
            ]
          [ contents ])
      graph
  in
  let total = Bonsai.map total ~f:(render_total 1. variant) in
  let label = label packed in
  let%arr breakdown = breakdown
  and total = total in
  let header = Vdom.Node.header [ Vdom.Node.text label ] in
  let breakdown =
    List.map Analysis.Hand_finger.all ~f:(fun hand_finger ->
      Map.find breakdown hand_finger
      |> Option.value ~default:(Vdom.Node.div [ Vdom.Node.none ]))
  in
  let total = Vdom.Node.create "total" [ total ] in
  (header :: breakdown) @ [ total ]
;;

let table
      (n : (Analysis.Hand_finger.t -> float) Bonsai.t)
      (t :
        ( Typed_variant.Packed.t
          , t
          , Typed_variant.Packed.comparator_witness )
          Map_intf.Map.t
          Bonsai.t)
      metrics_order
      graph
  =
  let data =
    Bonsai.assoc
      (module Typed_variant.Packed)
      t
      ~f:(fun _key data graph ->
        match%sub data with
        | Sfb metric -> row_1 n Sfb ~metric graph
        | Sfs metric -> row_1 n Sfs ~metric graph
        | Speed metric -> row_1 n Speed ~metric graph
        | Sfb_worst metric -> row_2 n Sfb_worst ~metric graph
        | Sfs_worst metric -> row_2 n Sfs_worst ~metric graph
        | Speed_worst metric -> row_2 n Speed_worst ~metric graph)
      graph
  in
  let%arr data = data in
  let header =
    let all = Analysis.Hand_finger.all in
    let to_string = Analysis.Hand_finger.to_string in
    (all |> List.map ~f:to_string) @ [ "Total" ]
  in
  Vdom.Node.div (*table*)
    ~attrs:[ grid_attr ~padding:"0" 10 ]
    (let header =
       Vdom.Node.header (*th*) ~attrs:[] [ (* empty corner cell *) ]
       :: List.map header ~f:(fun label ->
         Vdom.Node.header (*th*) [ Vdom.Node.text label ])
     in
     let rows =
       List.map metrics_order ~f:(fun metric -> Map.find data metric) |> List.filter_opt
     in
     let grid = header :: rows in
     List.concat grid)
;;

let component ~keyboard ~finger_dexterity ~corpus graph =
  let worst_counter, worst_counter_vdom =
    let n, inject = Analysis.Counter.counter 6 graph in
    let vdom = Counter.vdom ~n ~inject ~msg:(fun n -> sprintf "%d" n) in
    n, vdom
  in
  let controls =
    let%arr controls = multiselect graph
    and worst_counter_vdom = worst_counter_vdom in
    let worst_counter_vdom =
      Vdom.Node.div
        ~attrs:
          [ [%css
              {|
                display: flex;
                flex-direction: column;
              |}]
          ]
        [ Vdom.Node.label [ Vdom.Node.text "Worst Counter" ]; worst_counter_vdom ]
    in
    Form.map_view controls ~f:(fun vdom ->
      Vdom.Node.div
        ~attrs:
          [ Design.Card.attr
          ; [%css
              {|
                display: flex;
                flex-direction: column;
                gap: 1rem;
              |}]
          ]
        [ vdom; worst_counter_vdom ])
  in
  let metrics =
    let%arr controls = controls in
    Form.value_or_default
      controls
      ~default:(Set.of_list (module Typed_variant.Packed) Typed_variant.Packed.all)
  in
  let diff_row_bigram_data =
    let data = Analysis.Bigram_data.make keyboard corpus graph in
    bigram_data data graph
  in
  let stats_same_finger =
    Analysis.Stats_same_finger.component
      ~metrics
      ~worst_counter
      ~diff_row_bigram_data
      graph
  in
  let table =
    table
      finger_dexterity
      stats_same_finger
      Analysis.Stats_same_finger.Typed_variant.Packed.all
      graph
  in
  let vdom =
    let%arr metrics = metrics
    and table = table in
    if Set.is_empty metrics
    then Vdom.Node.none
    else Vdom.Node.div ~attrs:[ Design.Card.attr ] [ table ]
  in
  stats_same_finger, controls, vdom
;;
