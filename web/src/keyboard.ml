open! Import

type t = char Int.Map.t [@@deriving sexp, compare]

let of_string s : t =
  s |> String.to_list |> List.mapi ~f:(fun i c -> i, c) |> Int.Map.of_alist_exn
;;

let cell c ~monograms =
  let open Incr_dom.Vdom in
  let alpha =
    match monograms with
    | None -> 0.
    | Some monograms -> Hashtbl.find_or_add monograms c ~default:(fun () -> 0.)
  in
  let color =
    Css_gen.Color.RGBA.create
      ~r:255
      ~g:0
      ~b:0
      ~a:(Percent.of_percentage (alpha *. 255.))
      ()
  in
  let attr =
    Attr.many [ Attr.class_ "k"; Attr.style (Css_gen.background_color (`RGBA color)) ]
  in
  Node.div ~attr [ Node.text (Char.to_string c) ]
;;

let row p keyboard ~monograms =
  let open Incr_dom.Incr.Let_syntax in
  let open Incr_dom.Vdom in
  Incr_map.filter_mapi keyboard ~f:(fun ~key:i ~data:c -> if p i then Some c else None)
  |> Incr_map.map ~f:(cell ~monograms)
  |> Incr_dom.Incr.map ~f:(fun r ->
         Map.data r
         |> List.concat_mapi ~f:(fun i v ->
                if i = 5 then [ v; Node.div ~attr:(Attr.class_ "e") [] ] else [ v ]))
;;

let view keyboard =
  let open Incr_dom.Incr.Let_syntax in
  let open Incr_dom.Vdom in
  let monograms =
    match Analyzer.Incr.node_value Analyzer.Corpus.monograms with
    | Invalid -> None
    | Necessary_maybe_stale x | Unnecessary_maybe_stale x -> x
  in
  let%map top = row (fun i -> i < 10) keyboard ~monograms
  and middle = row (fun i -> i >= 10 && i < 20) keyboard ~monograms
  and bottom = row (fun i -> i >= 20 && i < 30) keyboard ~monograms in
  Node.div ~attr:(Attr.id "keyboard") (top @ middle @ bottom)
;;