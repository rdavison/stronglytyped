open! Import

module T = struct
  type t = string * Stem.Keyboard.t [@@deriving sexp, compare, equal]

  let to_string (name, _) = name
end

include T

let state_machine default graph =
  let state, set_state = Bonsai.state' default graph in
  let set name keyboard =
    let%arr set_state = set_state in
    set_state (fun state -> List.Assoc.add ~equal:String.equal state name keyboard)
  in
  let del name =
    let%arr set_state = set_state in
    set_state (fun state -> List.Assoc.remove ~equal:String.equal state name)
  in
  state, set, del
;;

let component t ~set_keyboard ~theme graph =
  Listview.component
    (module T)
    t
    ~equal:[%equal: string * Stem.Keyboard.t]
    ~callback:
      (let%arr set_keyboard = set_keyboard in
       fun name_keyboard ->
         match name_keyboard with
         | None -> Ui_effect.Ignore
         | Some (_name, keyboard) -> set_keyboard keyboard)
    ~f:Fn.id
    ~theme
    graph
;;
