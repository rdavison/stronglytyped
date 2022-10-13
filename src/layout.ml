open! Import

let all =
  [ "qwerty", "qwertyuiopasdfghjkl;zxcvbnm,./"
  ; "dvorak", "',.pyfgcrlaoeuidhtns;qjkxbmwvz"
  ; "mtvap", "ypoujkdlcwinea,mhtsrqz'.;bfvgx"
  ; "alphabet", "abcdefghijklmnopqrstuvwxyz'.,;"
  ; "whorf", "flhdmvwou,srntkgyaeixjbzqpc';."
  ]
;;

let set v =
  let layout =
    match v with
    | `Layout layout -> layout
    | `Name name ->
      List.find_map_exn all ~f:(fun (name', layout) ->
          if String.equal name name' then Some layout else None)
  in
  String.iteri layout ~f:(fun i c -> Incr.Var.set Root.all.(i) c)
;;

let best_v : (float * string) list Incr.Var.t = Incr.Var.create []
let best = Incr.Var.watch best_v

let set_best layouts =
  Incr.Var.set best_v layouts;
  match layouts with
  | (_, x) :: _ -> set (`Layout x)
  | _ -> ()
;;

let set_next x = set (`Layout x)
