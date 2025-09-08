open! Core

module Action = struct
  type t =
    | Increment
    | Decrement
  [@@deriving sexp]
end

let counter n graph =
  Bonsai.state_machine
    ~default_model:n
    ~apply_action:(fun _ model (action : Action.t) ->
      let model =
        match action with
        | Increment -> model + 1
        | Decrement -> model - 1
      in
      if model <= 0 then 0 else model)
    graph
;;
