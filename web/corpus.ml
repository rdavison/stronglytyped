open! Core
open! Bonsai_web_proc
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form
module Corpus = Stronglytyped_analysis.Corpus
module Key = Stronglytyped_analysis.Key

module T = struct
  type t =
    | Foo
    | Custom
  [@@deriving sexp, equal, enumerate]
end

include T

(* let better_component = *)
(*   let%sub dropdown, dropdown_vdom = *)
(*     let%sub form = Form.Elements.Dropdown.list (module T) (Value.return T.all) in *)
(*     let%arr form = form in *)
(*     let value = form |> Form.value_or_default ~default:T.Foo in *)
(*     let vdom = Form.view_as_vdom ~editable:`Yes_always form in *)
(*     value, vdom *)
(*   in *)
(*   let%sub textarea, textarea_vdom = *)
(*     let extra_attrs = *)
(*       let%map dropdown = dropdown in *)
(*       match dropdown with *)
(*       | Foo -> [ Vdom.Attr.disabled ] *)
(*       | Custom -> [] *)
(*     in *)
(*     let%sub form = Form.Elements.Textarea.string ~extra_attrs () in *)
(*     let%arr form = form in *)
(*     let value = form |> Form.value_or_default ~default:"" in *)
(*     let vdom = form |> Form.view_as_vdom in *)
(*     value, vdom *)
(*   in *)
(*   let%arr dropdown = dropdown *)
(*   and textarea = textarea *)
(*   and dropdown_vdom = dropdown_vdom *)
(*   and textarea_vdom = textarea_vdom in *)
(*   let corpus = *)
(*     match dropdown with *)
(*     | Custom -> Corpus.of_string textarea *)
(*     | Foo -> Corpus.foo *)
(*   in *)
(*   corpus, Vdom.Node.div [ dropdown_vdom; textarea_vdom ] *)
(* ;; *)

let simple_component =
  let%sub corpus, set_corpus = Bonsai.state Corpus.empty in
  let%arr corpus = corpus
  and set_corpus = set_corpus in
  let vdom =
    Vdom.Node.textarea
      ~attrs:
        [ Vdom.Attr.on_change (fun _event data -> set_corpus (Corpus.of_string data)) ]
      []
  in
  corpus, vdom
;;

let component = simple_component
