open! Import

type t = Analyzer.Stats.t [@@deriving sexp, compare]

let view
    { Analyzer.Stats.sfb
    ; sfb_total
    ; dsfb
    ; dsfb_total
    ; speed
    ; speed_total
    ; lsb
    ; lsb_total
    ; keyfreq
    ; keyfreq_total
    ; roll
    ; roll_total
    ; roll_top
    ; roll_top_total
    ; roll_middle
    ; roll_middle_total
    ; roll_bottom
    ; roll_bottom_total
    ; roll_in
    ; roll_in_total
    ; roll_in_top
    ; roll_in_top_total
    ; roll_in_middle
    ; roll_in_middle_total
    ; roll_in_bottom
    ; roll_in_bottom_total
    ; roll_out
    ; roll_out_total
    ; roll_out_top
    ; roll_out_top_total
    ; roll_out_middle
    ; roll_out_middle_total
    ; roll_out_bottom
    ; roll_out_bottom_total
    ; dshrc
    ; dshrc_good
    ; dshrc_bad
    ; dshrc_total
    ; dshrc_good_total
    ; dshrc_bad_total
    }
  =
  let open Incr_dom.Vdom in
  let table data ~to_string ~all =
    let rows =
      let body =
        data
        |> List.map ~f:(fun (name, (data, total)) ->
               let data = Map.data data in
               Node.tr
                 (Node.td [ Node.text name ]
                 :: Node.td [ total |> Float.( * ) 100. |> sprintf "%.2f" |> Node.text ]
                 :: List.map data ~f:(fun col ->
                        Node.td [ col |> Float.( * ) 100. |> sprintf "%.2f" |> Node.text ])
                 ))
      in
      let header =
        let cols =
          Node.td [ Node.text "" ]
          :: Node.td [ Node.text "(total)" ]
          :: List.map all ~f:(fun x -> Node.td [ Node.text (to_string x) ])
        in
        printf "header: %d\n" (List.length cols);
        Node.tr cols
      in
      header :: body
    in
    Node.table rows
  in
  let hf_table =
    let open Analyzer in
    table
      ~all:Hf.all
      ~to_string:Hf.to_string
      [ "sfb", (sfb, sfb_total)
      ; "dsfb", (dsfb, dsfb_total)
      ; "speed", (speed, speed_total)
      ; "weight", (keyfreq, keyfreq_total)
      ]
  in
  let hand_table =
    let open Analyzer in
    table
      ~all:Hand.all
      ~to_string:Hand.to_string
      [ "lsb", (lsb, lsb_total)
      ; "dshrc", (dshrc, dshrc_total)
      ; "(good) dshrc", (dshrc_good, dshrc_good_total)
      ; "(bad) dshrc", (dshrc_bad, dshrc_bad_total)
      ; "roll", (roll, roll_total)
      ; " (in) roll", (roll_in, roll_in_total)
      ; "(out) roll", (roll_out, roll_out_total)
      ; "(top)       roll", (roll_top, roll_top_total)
      ; "(middle)       roll", (roll_middle, roll_middle_total)
      ; "(bottom)       roll", (roll_bottom, roll_bottom_total)
      ; "(top)  (in) roll", (roll_in_top, roll_in_top_total)
      ; "(middle)  (in) roll", (roll_in_middle, roll_in_middle_total)
      ; "(bottom)  (in) roll", (roll_in_bottom, roll_in_bottom_total)
      ; "(top) (out) roll", (roll_out_top, roll_out_top_total)
      ; "(middle) (out) roll", (roll_out_middle, roll_out_middle_total)
      ; "(bottom) (out) roll", (roll_out_bottom, roll_out_bottom_total)
      ]
  in
  let sections = [ "hand-finger", hf_table; "hand", hand_table ] in
  Node.div
    (List.map sections ~f:(fun (name, node) ->
         Node.div [ Node.h3 [ Node.text name ]; node ]))
;;
