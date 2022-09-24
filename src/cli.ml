open! Core

module P = struct
  open Command.Param

  let total = anon ("total" %: int)
end

let main =
  Command.basic
    ~summary:"Ypou Nercds"
    (let%map_open.Command total = P.total
     and config = return (Config.create ()) in
     fun () ->
       let observations, span = Opt.anneal ~kmax:total ~config in
       printf
         "Analyzed %s layouts, observed changes %s times. Time elapsed: %s\n"
         (Int.to_string_hum total)
         (Int.to_string_hum observations)
         (Time.Span.to_string_hum span))
;;
