open! Core
open! Import
open! Bonsai_web
open! Bonsai.Let_syntax
module Hand_finger = Stronglytyped_analysis.Hand_finger
module Key = Stronglytyped_analysis.Key
module Keyboard = Stronglytyped_analysis.Keyboard
module Corpus = Stronglytyped_analysis.Corpus

let render_speed x = sprintf "%.2fd/t" (Float.abs (x *. 100.))
let render_freq x = sprintf "%.2f%%" (Float.abs (x *. 100.))

let row label (map, total) to_vdom to_vdom2 all =
  let cols = all |> List.map ~f:(fun x -> x |> Map.find map |> to_vdom) in
  label, cols @ [ to_vdom2 total ]
;;

let table header data =
  let module N = Vdom.Node in
  let scope x = Vdom.Attr.create "scope" x in
  N.table
    ~attrs:[]
    [ N.thead
        [ N.tr
            (N.th ~attrs:[] [ (* empty corner cell *) ]
             :: List.map header ~f:(fun label ->
               N.th ~attrs:[ scope "col" ] [ N.text label ]))
        ]
    ; N.tbody
        (List.map data ~f:(fun (label, row) ->
           N.tr
             (N.th ~attrs:[ scope "row" ] [ label ]
              :: List.map row ~f:(fun cell -> N.td ~attrs:[] [ cell ]))))
    ]
;;

let same_finger_entry_to_vdom entry =
  entry |> Option.value ~default:(Vdom.Node.text "[Not Found]")
;;

let counter n msg =
  let module Action = struct
    type t =
      | Increment
      | Decrement
    [@@deriving sexp]
  end
  in
  let%sub n, inject =
    Bonsai.state_machine0
      (module Int)
      (module Action)
      ~default_model:n
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        let model =
          match action with
          | Increment -> model + 1
          | Decrement -> model - 1
        in
        if model <= 0 then 0 else model)
  in
  let%arr n = n
  and inject = inject in
  let button label (action : Action.t) =
    let disabled =
      match action with
      | Decrement when n <= 0 -> [ Vdom.Attr.disabled ]
      | _ -> []
    in
    Vdom.Node.div
      ~attrs:
        ([ Style.counter_button; Vdom.Attr.on_click (fun _event -> inject action) ]
         @ disabled)
      [ Vdom.Node.text label ]
  in
  let vdom =
    Vdom.Node.div
      ~attrs:[ Style.counter_container ]
      [ button "-" Decrement
      ; Vdom.Node.div ~attrs:[ Style.counter ] [ Vdom.Node.text (msg n) ]
      ; button "+" Increment
      ]
  in
  n, vdom
;;

let same_finger_stat_vdom what render =
  let%sub breakdown =
    Bonsai.assoc
      (module Hand_finger)
      what
      ~f:(fun _hand_finger freq ->
        let%arr freq = freq in
        Vdom.Node.text (render freq))
  in
  let%sub sum = Bonsai.Map.sum what (module Float) ~f:Fn.id in
  let%arr breakdown = breakdown
  and sum = sum in
  breakdown, Vdom.Node.text (render sum)
;;

let component keyboard corpus worst_counter =
  let%sub same_hand_same_finger_diff_row_bigram_data =
    let%sub same_hand_same_finger_bigram_data =
      let%sub bigram_data = Bigram_data.make keyboard corpus in
      Bonsai.Map.index_byi
        bigram_data
        ~comparator:(module Hand_finger)
        ~index:(fun ~key:(a, b) ~data:_ ->
          let x = Key.Id.hand a, Key.Id.finger a in
          if Hand_finger.equal x (Key.Id.hand b, Key.Id.finger b) then Some x else None)
    in
    Bonsai.assoc
      (module Hand_finger)
      same_hand_same_finger_bigram_data
      ~f:(fun _ (bigram_data : Bigram_data.t Value.t) ->
        Bonsai.Map.filter_mapi bigram_data ~f:(fun ~key:(a, b) ~data ->
          if Int.equal (Key.Id.row a) (Key.Id.row b) then None else Some data))
  in
  let same_finger_metrics_vdom render_metric compute_metric =
    let%sub sum_and_worst =
      Bonsai.assoc
        (module Hand_finger)
        same_hand_same_finger_diff_row_bigram_data
        ~f:(fun _ bigram_data ->
          let%sub sum = Bonsai.Map.sum bigram_data (module Float) ~f:compute_metric in
          let%arr sum = sum
          and worst_counter = worst_counter
          and bigram_data = bigram_data in
          let worst_n =
            bigram_data
            |> Map.to_alist
            |> List.map ~f:(fun (_, bigram_info) ->
              bigram_info.corpus_key, compute_metric bigram_info)
            |> List.sort ~compare:(fun (_, a) (_, b) -> Float.compare b a)
            |> Fn.flip List.take worst_counter
          in
          sum, worst_n)
    in
    let%sub hand_finger_freq =
      Bonsai.assoc
        (module Hand_finger)
        sum_and_worst
        ~f:(fun _ sum_and_worst ->
          let%arr sum, _ = sum_and_worst in
          sum)
    in
    let%sub worst_vdom =
      let%sub breakdown =
        Bonsai.assoc
          (module Hand_finger)
          sum_and_worst
          ~f:(fun _ sum_and_worst ->
            let%arr _, worst = sum_and_worst in
            Vdom.Node.div
              ~attrs:[ Style.stats_vertical_analysis_inner_table ]
              [ Vdom.Node.table
                  (List.map worst ~f:(fun (corpus_key, freq) ->
                     Vdom.Node.tr
                       [ Vdom.Node.td [ Vdom.Node.text corpus_key ]
                       ; Vdom.Node.td [ Vdom.Node.text (render_metric freq) ]
                       ]))
              ])
      in
      let%sub sum =
        Bonsai.Map.sum
          sum_and_worst
          (module Float)
          ~f:(fun (_, x) -> List.sum (module Float) x ~f:(fun (_, x) -> x))
      in
      let%arr breakdown = breakdown
      and sum = sum in
      breakdown, Vdom.Node.text (render_metric sum)
    in
    let%sub same_finger_stat_by_finger_vdom =
      same_finger_stat_vdom hand_finger_freq render_metric
    in
    let%arr same_finger_stat_by_finger_vdom = same_finger_stat_by_finger_vdom
    and worst_vdom = worst_vdom in
    same_finger_stat_by_finger_vdom, worst_vdom
  in
  let%sub sfb_metrics_vdom =
    same_finger_metrics_vdom render_freq (fun (bigram_info : Bigram_data.info) ->
      bigram_info.freqs.ab)
  in
  let%sub sfs_metrics_vdom =
    same_finger_metrics_vdom render_freq (fun (bigram_info : Bigram_data.info) ->
      bigram_info.freqs.axc)
  in
  let%sub speed_metrics_vdom =
    same_finger_metrics_vdom render_speed (fun (bigram_info : Bigram_data.info) ->
      bigram_info.dist *. (bigram_info.freqs.ab +. (0.5 *. bigram_info.freqs.axc)))
  in
  let%arr sfb_by_finger_vdom, sfb_worst_vdom = sfb_metrics_vdom
  and sfs_by_finger_vdom, sfs_worst_vdom = sfs_metrics_vdom
  and speed_by_finger_vdom, speed_worst_vdom = speed_metrics_vdom in
  let vertical_analysis =
    let all = Hand_finger.all in
    let all_to_string = Hand_finger.to_string in
    let header = (all |> List.map ~f:all_to_string) @ [ "Total" ] in
    let same_finger_entry_to_vdom = same_finger_entry_to_vdom in
    let data =
      [ row
          (Vdom.Node.text "Speed")
          speed_by_finger_vdom
          same_finger_entry_to_vdom
          Fn.id
          all
      ; row (Vdom.Node.text "SFB") sfb_by_finger_vdom same_finger_entry_to_vdom Fn.id all
      ; row (Vdom.Node.text "SFS") sfs_by_finger_vdom same_finger_entry_to_vdom Fn.id all
      ; row
          (Vdom.Node.text "Worst Speed")
          speed_worst_vdom
          same_finger_entry_to_vdom
          Fn.id
          all
      ; row
          (Vdom.Node.text "Worst SFB")
          sfb_worst_vdom
          same_finger_entry_to_vdom
          Fn.id
          all
      ; row
          (Vdom.Node.text "Worst SFS")
          sfs_worst_vdom
          same_finger_entry_to_vdom
          Fn.id
          all
      ]
    in
    Vdom.Node.div ~attrs:[ Style.stats_vertical_analysis ] [ table header data ]
  in
  Vdom.Node.div [ Vdom.Node.h2 [ Vdom.Node.text "Vertical Analysis" ]; vertical_analysis ]
;;
