let init () =
  let open! Js_of_ocaml in
  let open! Js_of_ocaml_lwt in
  let ( >>= ) = Lwt.bind in
  let http_get url =
    XmlHttpRequest.get url
    >>= fun r ->
    let cod = r.XmlHttpRequest.code in
    let msg = r.XmlHttpRequest.content in
    if cod = 0 || cod = 200 then Lwt.return msg else fst (Lwt.wait ())
  in
  Lwt.on_success (http_get "/static/corpus/data.sexp") (fun s -> Ypou.Corpus.set_data s)
;;

let () = init ()
