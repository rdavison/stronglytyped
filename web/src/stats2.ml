open! Import

type 'data t =
  { hf : ('data Analyzer.Hf.Map.t * 'data) String.Map.t
  ; hand : ('data Analyzer.Hand.Map.t * 'data) String.Map.t
  }
[@@deriving sexp, compare]

let mapi2 (stats_a : float t) (stats_b : float t) ~f =
  let f ma mb =
    Map.merge ma mb ~f:(fun ~key merge_elem ->
        Option.both
          (Map.Merge_element.left merge_elem)
          (Map.Merge_element.right merge_elem)
        |> Option.map ~f:(fun (ma, mb) ->
               let map_a, total_a = ma in
               let map_b, total_b = mb in
               let map_ab =
                 Map.merge map_a map_b ~f:(fun ~key:_ merge_elem ->
                     Option.both
                       (Map.Merge_element.left merge_elem)
                       (Map.Merge_element.right merge_elem))
               in
               let total_ab = total_a, total_b in
               Map.map map_ab ~f:(fun data -> f ~key ~data), f ~key ~data:total_ab))
  in
  { hf = f stats_a.hf stats_b.hf; hand = f stats_a.hand stats_b.hand }
;;

let diff_color prev curr =
  let const (_prev, curr) = curr, None in
  let lo (prev, curr) =
    let cmp = Float.compare prev curr in
    if cmp < 0
    then curr, Some (`Name "green")
    else if cmp > 0
    then curr, Some (`Name "red")
    else const (prev, curr)
  in
  let hi (prev, curr) = lo (curr, prev) in
  mapi2 prev curr ~f:(fun ~key ~data ->
      match key with
      | "sfb" -> lo data
      | "dsfb" -> lo data
      | "speed" -> lo data
      | "lsb" -> lo data
      | "keyfreq" -> lo data
      | "roll" -> hi data
      | "roll_top" -> hi data
      | "roll_middle" -> hi data
      | "roll_bottom" -> hi data
      | "roll_in" -> hi data
      | "roll_in_top" -> hi data
      | "roll_in_middle" -> hi data
      | "roll_in_bottom" -> hi data
      | "roll_out" -> hi data
      | "roll_out_top" -> hi data
      | "roll_out_middle" -> hi data
      | "roll_out_bottom" -> hi data
      | "dshrc" -> lo data
      | "dshrc_good" -> lo data
      | "dshrc_bad" -> lo data
      | other -> const data)
;;

let view { hf; hand } to_node =
  let open Incr_dom.Vdom in
  let table data ~to_string ~all =
    let rows =
      let body =
        data
        |> Map.to_alist
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
        Node.tr cols
      in
      header :: body
    in
    Node.table rows
  in
  let hf_table =
    let open Analyzer in
    table ~all:Hf.all ~to_string:Hf.to_string hf
  in
  let hand_table =
    let open Analyzer in
    table ~all:Hand.all ~to_string:Hand.to_string hand
  in
  let sections = [ "hand-finger", hf_table; "hand", hand_table ] in
  Node.div
    (List.map sections ~f:(fun (name, node) ->
         Node.div [ Node.h3 [ Node.text name ]; node ]))
;;
