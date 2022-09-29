open! Import

type t =
  { s1 : float String.Table.t
  ; s2 : float String.Table.t
  ; s3 : float String.Table.t
  ; s4 : float String.Table.t
  ; s5 : float String.Table.t
  ; s6 : float String.Table.t
  ; s7 : float String.Table.t
  ; s8 : float String.Table.t
  ; s9 : float String.Table.t
  ; singles : float Char.Table.t
  ; triples : float String.Table.t
  }
[@@deriving sexp]

module Parse = struct
  type t =
    { s1 : float String.Table.t option
    ; s2 : float String.Table.t option
    ; s3 : float String.Table.t option
    ; s4 : float String.Table.t option
    ; s5 : float String.Table.t option
    ; s6 : float String.Table.t option
    ; s7 : float String.Table.t option
    ; s8 : float String.Table.t option
    ; s9 : float String.Table.t option
    ; singles : float Char.Table.t option
    ; triples : float String.Table.t option
    }
  [@@deriving sexp]

  let data_v = Incr.Var.create ""
  let set_data = Incr.Var.set data_v
  let data = Incr.Var.watch data_v

  let incr =
    let%bind.Incr data = data in
    let v = t_of_sexp (Sexp.of_string data) in
    Incr.return v
  ;;
end

let set_data = Parse.set_data

let incr =
  Incr.map
    Parse.incr
    ~f:(fun { Parse.s1; s2; s3; s4; s5; s6; s7; s8; s9; singles; triples } ->
      { s1 = Option.value_exn s1
      ; s2 = Option.value_exn s2
      ; s3 = Option.value_exn s3
      ; s4 = Option.value_exn s4
      ; s5 = Option.value_exn s5
      ; s6 = Option.value_exn s6
      ; s7 = Option.value_exn s7
      ; s8 = Option.value_exn s8
      ; s9 = Option.value_exn s9
      ; singles = Option.value_exn singles
      ; triples = Option.value_exn triples
      })
;;
