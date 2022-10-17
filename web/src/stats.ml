open! Import

type 'a t = 'a Analyzer.Stats.t

let worst = Analyzer.Stats.worst
let both = Analyzer.Stats.both

module Vio = struct
  type 'a t =
    { v : 'a Incr_dom.Incr.Var.t
    ; i : 'a Incr_dom.Incr.t
    ; o : 'a Incr_dom.Incr.Observer.t
    }

  let make ?cutoff data =
    let open Incr_dom in
    let var = Incr.Var.create data in
    let incr = Incr.Var.watch var in
    let observer = Incr.observe incr in
    Option.iter cutoff ~f:(Incr.set_cutoff incr);
    var, incr, observer
  ;;

  module Internal = struct
    let hf v =
      Analyzer.Hf.all |> List.map ~f:(fun key -> key, v) |> Analyzer.Hf.Map.of_alist_exn
    ;;

    let hand v =
      Analyzer.Hand.all
      |> List.map ~f:(fun key -> key, v)
      |> Analyzer.Hand.Map.of_alist_exn
    ;;

    let max = Float.max_finite_value
    let min = 0.
  end

  open Internal

  let sfb = hf max |> make
  let sfb_total = max |> make
  let dsfb = hf max |> make
  let dsfb_total = max |> make
  let speed = hf max |> make
  let speed_total = max |> make
  let lsb = hand max |> make
  let lsb_total = max |> make
  let keyfreq = hf max |> make
  let keyfreq_total = max |> make
  let roll = hand min |> make
  let roll_total = min |> make
  let roll_top = hand min |> make
  let roll_top_total = min |> make
  let roll_middle = hand min |> make
  let roll_middle_total = min |> make
  let roll_bottom = hand min |> make
  let roll_bottom_total = min |> make
  let roll_in = hand min |> make
  let roll_in_total = min |> make
  let roll_in_top = hand min |> make
  let roll_in_top_total = min |> make
  let roll_in_middle = hand min |> make
  let roll_in_middle_total = min |> make
  let roll_in_bottom = hand min |> make
  let roll_in_bottom_total = min |> make
  let roll_out = hand min |> make
  let roll_out_total = min |> make
  let roll_out_top = hand min |> make
  let roll_out_top_total = min |> make
  let roll_out_middle = hand min |> make
  let roll_out_middle_total = min |> make
  let roll_out_bottom = hand min |> make
  let roll_out_bottom_total = min |> make
  let dshrc = hand max |> make
  let dshrc_total = max |> make
  let dshrc_good = hand min |> make
  let dshrc_good_total = min |> make
  let dshrc_bad = hand max |> make
  let dshrc_bad_total = max |> make
end

let diff_color
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
  let lo (a, b) =
    if Float.compare a b < 0
    then Some (`Name "green")
    else if Float.compare a b > 0
    then None
    else Some (`Name "red")
  in
  let hi (b, a) = lo (a, b) in
  let f c (prev, curr) =
    let color = c (prev, curr) in
    curr, color
  in
  let map g c x = Map.map x ~f:(g c) in
  { Analyzer.Stats.sfb = map f lo sfb
  ; sfb_total = f lo sfb_total
  ; dsfb = map f lo dsfb
  ; dsfb_total = f lo dsfb_total
  ; speed = map f lo speed
  ; speed_total = f lo speed_total
  ; lsb = map f lo lsb
  ; lsb_total = f lo lsb_total
  ; keyfreq = map f lo keyfreq
  ; keyfreq_total = f lo keyfreq_total
  ; roll = map f hi roll
  ; roll_total = f hi roll_total
  ; roll_top = map f hi roll_top
  ; roll_top_total = f hi roll_top_total
  ; roll_middle = map f hi roll_middle
  ; roll_middle_total = f hi roll_middle_total
  ; roll_bottom = map f hi roll_bottom
  ; roll_bottom_total = f hi roll_bottom_total
  ; roll_in = map f hi roll_in
  ; roll_in_total = f hi roll_in_total
  ; roll_in_top = map f hi roll_in_top
  ; roll_in_top_total = f hi roll_in_top_total
  ; roll_in_middle = map f hi roll_in_middle
  ; roll_in_middle_total = f hi roll_in_middle_total
  ; roll_in_bottom = map f hi roll_in_bottom
  ; roll_in_bottom_total = f hi roll_in_bottom_total
  ; roll_out = map f hi roll_out
  ; roll_out_total = f hi roll_out_total
  ; roll_out_top = map f hi roll_out_top
  ; roll_out_top_total = f hi roll_out_top_total
  ; roll_out_middle = map f hi roll_out_middle
  ; roll_out_middle_total = f hi roll_out_middle_total
  ; roll_out_bottom = map f hi roll_out_bottom
  ; roll_out_bottom_total = f hi roll_out_bottom_total
  ; dshrc = map f lo dshrc
  ; dshrc_good = map f lo dshrc_good
  ; dshrc_bad = map f lo dshrc_bad
  ; dshrc_total = f lo dshrc_total
  ; dshrc_good_total = f lo dshrc_good_total
  ; dshrc_bad_total = f lo dshrc_bad_total
  }
;;

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
    to_node
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
                 :: Node.td [ to_node total ]
                 :: List.map data ~f:(fun col -> Node.td [ to_node col ])))
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
