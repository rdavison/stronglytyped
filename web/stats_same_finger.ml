open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view
include Analysis.Stats_same_finger

let multiselect graph =
  let%arr form =
    Form.Elements.Multiselect.set
      (module Typed_variant.Packed)
      (Bonsai.return Typed_variant.Packed.all)
      graph
  in
  Form.map_view form ~f:(fun view ->
    Vdom.Node.div
      ~attrs:
        [ [%css
            {|
              padding: 1rem;
              border: 1px solid %{Tailwind_v3_colors.slate900#Css_gen.Color};
              background-color: %{Tailwind_v3_colors.slate100#Css_gen.Color};
              box-shadow: 0 1px 2px rgba(0,0,0,.4);

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

let render_speed x = sprintf "%.2fd/t" (Float.abs (x *. 100.))
let render_freq x = sprintf "%.2f%%" (Float.abs (x *. 100.))

let render_measurement (t : Typed_variant.Packed.t) =
  match t.f with
  | T Sfb -> render_freq
  | T Sfs -> render_freq
  | T Speed -> render_speed
  | T Sfb_worst -> render_freq
  | T Sfs_worst -> render_freq
  | T Speed_worst -> render_speed
;;

let render_simple (t : (float, 'total) metric Typed_variant.t) data =
  let packed = Typed_variant.Packed.pack t in
  Vdom.Node.text (render_measurement packed data)
;;

let css_attr_prev_curr prev curr =
  match prev with
  | None -> Vdom.Attr.empty
  | Some prev ->
    let diff = curr -. prev in
    (match Float.compare prev curr with
     | 0 -> Vdom.Attr.empty
     | _ ->
       let (`RGB (r, g, b)) =
         Util.Color.convert_hex_to_rgb
           (if Float.( > ) diff 0.
            then Tailwind_v3_colors.green500
            else Tailwind_v3_colors.red500)
       in
       (* let scale x = Float.tanh (50. *. Float.sin (Float.pi *. x /. 2.)) in *)
       let scale x = 10. *. x in
       let a = Percent.of_mult (scale (Float.abs curr)) in
       let color = `RGBA (Css_gen.Color.RGBA.create ~r ~g ~b ~a ()) in
       [%css {|background-color: %{color#Css_gen.Color};|}])
;;

let render_prev_curr
      (t : (float option * float, 'total) metric Typed_variant.t)
      ((prev, curr) : float option * float)
  =
  let packed = Typed_variant.Packed.pack t in
  Vdom.Node.div
    ~attrs:[ css_attr_prev_curr prev curr ]
    [ Vdom.Node.text (render_measurement packed curr) ]
;;

let table_color = Tailwind_v3_colors.slate900

let grid_attr cols =
  let cols = Int.to_string cols in
  [%css
    {|
      display: grid;
      grid-template-columns: repeat(%{cols}, 1fr);
      gap: 1px;
      background: %{Tailwind_v3_colors.slate600#Css_gen.Color};
      border-radius: 12px;
      overflow: hidden;

      & > div {
        background: %{Tailwind_v3_colors.slate700#Css_gen.Color};
        padding: 0.5rem;
        text-align: center;
        font-family: monospace;
      }
    |}]
;;

let render_detailed (t : ((string * float) list, 'total) metric Typed_variant.t) worst =
  let packed = Typed_variant.Packed.pack t in
  Vdom.Node.div
    ~attrs:[]
    [ Vdom.Node.div
        ~attrs:[ grid_attr 2 ]
        (List.concat_map worst ~f:(fun (corpus_key, metric) ->
           [ Vdom.Node.div [ Vdom.Node.text corpus_key ]
           ; Vdom.Node.div [ Vdom.Node.text (render_measurement packed metric) ]
           ]))
    ]
;;

let render_breakdown
  : type breakdown. (breakdown, 'total) metric Typed_variant.t -> breakdown -> Vdom.Node.t
  =
  fun t breakdown ->
  match t with
  | Sfb -> render_prev_curr Sfb breakdown
  | Sfs -> render_prev_curr Sfs breakdown
  | Speed -> render_simple Speed breakdown
  | Sfb_worst -> render_detailed Sfb_worst breakdown
  | Sfs_worst -> render_detailed Sfs_worst breakdown
  | Speed_worst -> render_detailed Speed_worst breakdown
;;

let render_total t total = Vdom.Node.text (render_measurement t total)

let row
      (t : ('breakdown, 'total) metric Typed_variant.t)
      ~(metric : ('breakdown, 'total) metric Bonsai.t)
      graph
  : Vdom.Node.t list Bonsai.t
  =
  let packed = Typed_variant.Packed.pack t in
  let%sub { breakdown; total } = metric in
  let breakdown =
    Bonsai.assoc
      (module Analysis.Hand_finger)
      breakdown
      ~f:(fun _key data _graph ->
        let contents =
          let%arr data = data in
          render_breakdown t data
        in
        let%arr contents = contents in
        Vdom.Node.div [ contents ])
      graph
  in
  let total = Bonsai.map total ~f:(render_total packed) in
  let label = label packed in
  let%arr breakdown = breakdown
  and total = total in
  let header =
    Vdom.Node.div ~attrs:[ [%css {|font-weight: bold;|}] ] [ Vdom.Node.text label ]
  in
  let breakdown =
    List.map Analysis.Hand_finger.all ~f:(fun hand_finger ->
      Map.find breakdown hand_finger
      |> Option.value ~default:(Vdom.Node.div [ Vdom.Node.none ]))
  in
  let total = Vdom.Node.div [ total ] in
  (header :: breakdown) @ [ total ]
;;

let table
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
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
            padding: 0.25rem;
            background: transparent;
          |}]
      ]
    [ Vdom.Node.div (*table*)
        ~attrs:[ grid_attr 10 ]
        (let header =
           Vdom.Node.div (*th*) ~attrs:[] [ (* empty corner cell *) ]
           :: List.map header ~f:(fun label ->
             Vdom.Node.div (*th*)
               ~attrs:[ [%css {|font-weight: bold;|}] ]
               [ Vdom.Node.text label ])
         in
         let rows =
           List.map metrics_order ~f:(fun metric -> Map.find data metric)
           |> List.filter_opt
         in
         let grid = header :: rows in
         List.concat grid)
    ]
;;

let component ~keyboard ~corpus ~worst_counter graph =
  let controls = multiselect graph in
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
    table stats_same_finger Analysis.Stats_same_finger.Typed_variant.Packed.all graph
  in
  let vdom =
    let%arr metrics = metrics
    and table = table in
    if Set.is_empty metrics
    then Vdom.Node.none
    else
      Vdom.Node.div
        ~attrs:
          [ [%css
              {|
              background: %{table_color#Css_gen.Color};
              color: %{Tailwind_v3_colors.neutral200#Css_gen.Color};
              border: 1px solid #e7e9ee;
              border-radius: 12px;
              padding: 16px;
              box-shadow: 0 1px 2px rgba(0,0,0,.4);
            |}]
          ]
        [ table ]
  in
  controls, vdom
;;
