open! Js_of_ocaml
open! Js_of_ocaml_lwt
open! Lwt.Syntax
module Html = Dom_html
module Incr = Ypou.Incr
open Core

let js = Js.string
let document = Html.window##.document

let button name callback =
  let res = document##createDocumentFragment in
  let input = Html.createInput ~_type:(js "submit") document in
  input##.value := js name;
  input##.onclick := Html.handler callback;
  Dom.appendChild res input;
  res
;;

let text data =
  let res = document##createDocumentFragment in
  let ta = Html.createTextarea document in
  let setText s = ta##.innerHTML := js s in
  setText data;
  Dom.appendChild res ta;
  res, setText
;;

let _float_input name value =
  let res = document##createDocumentFragment in
  Dom.appendChild res (document##createTextNode (js name));
  let input = Html.createInput ~_type:(js "text") document in
  Incr.Observer.on_update_exn
    (Incr.observe (Incr.Var.watch value))
    ~f:(fun obs ->
      match obs with
      | Initialized v | Changed (_, v) -> input##.value := js (string_of_float v)
      | Invalidated -> ());
  input##.onchange
    := Html.handler (fun _ ->
           let v =
             Result.try_with (fun () -> float_of_string (Js.to_string input##.value))
           in
           (match v with
           | Ok v ->
             Incr.Var.set value v;
             input##.value := js (string_of_float v)
           | Error _ -> ());
           Js._false);
  Dom.appendChild res input;
  res
;;

(* let init () =
  let http_get url =
    let* r = XmlHttpRequest.get url in
    let cod = r.XmlHttpRequest.code in
    let msg = r.XmlHttpRequest.content in
    if cod = 0 || cod = 200 then Lwt.return msg else fst (Lwt.wait ())
  in
  Lwt.on_success
    (http_get "/static/corpus/data.sexp")
    (Ypou.Incr.Var.set Ypou.Corpus.data_v)
;; *)

(* val c_sfb : float Incr.t
val c_dsfb : float Incr.t
val c_keyfreq : float Finger.Table.t Incr.t
val c_roll : float Incr.t
val c_lsb : float Incr.t
val monograms : float Char.Table.t Incr.t
val bigrams : float String.Table.t Incr.t
val skipgrams : float String.Table.t Incr.t
val neighbor : Neighbor.t Incr.t
val kmax : int Incr.t
val progress : float Incr.t 
val set_progress : float -> unit *)

let stabilize () = Lwt.wrap Incr.stabilize

let onload _ =
  let app = Js.Opt.get (document##getElementById (js "app")) (fun () -> assert false) in
  (* Dom.appendChild app (float_input "Sfb Weight" Ypou.Config.Var.C.sfb (fun _ -> ""));
  Dom.appendChild app (Html.createBr document);
  Dom.appendChild app (float_input "Dsfb Weight" Ypou.Config.Var.C.dsfb (fun _ -> ""));
  Dom.appendChild app (Html.createBr document); *)
  (* Dom.appendChild app (float_input "Roll Weight" Ypou.Config.Var.C.roll);
  Dom.appendChild app (Html.createBr document);
  Dom.appendChild app (float_input "Lsb Weight" Ypou.Config.Var.C.lsb);
  Dom.appendChild app (Html.createBr document); *)
  Dom.appendChild
    app
    (button "Load Corpus" (fun _ ->
         let div = Html.createDiv document in
         Dom.appendChild app div;
         (* init (); *)
         Js._false));
  Dom.appendChild
    app
    (button "Scramble" (fun _ ->
         let div = Html.createDiv document in
         Dom.appendChild app div;
         Ypou.Root.scramble 100;
         Js._false));
  Dom.appendChild
    app
    (button "Start" (fun _ ->
         let div = Html.createDiv document in
         Dom.appendChild app div;
         let f = Lwt.wrap (fun () -> Ypou.Cli.gen ()) in
         Lwt.dont_wait (fun () -> f) ignore;
         Js._false));
  Dom.appendChild
    app
    (button "Step" (fun _ ->
         let div = Html.createDiv document in
         Dom.appendChild app div;
         Lwt.dont_wait stabilize ignore;
         Js._false));
  let text, set_text = text "" in
  Dom.appendChild app text;
  Incr.Observer.on_update_exn (Incr.observe Ypou.Opt.anneal) ~f:(fun obs ->
      match obs with
      | Initialized (_, best :: _) | Changed (_, (_, best :: _)) ->
        set_text best.Ypou.Analysis.layout_pretty
      | _ -> ());
  Js._false
;;

let () = Html.window##.onload := Html.handler onload
