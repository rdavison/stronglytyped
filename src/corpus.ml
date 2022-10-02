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

let n tbl =
  let total = List.sum (module Float) (Hashtbl.data tbl) ~f:Fn.id in
  Hashtbl.map tbl ~f:(fun v -> v /. total)
;;

let set_data = Parse.set_data

let incr =
  Incr.map
    Parse.incr
    ~f:(fun { Parse.s1; s2; s3; s4; s5; s6; s7; s8; s9; singles; triples } ->
      { s1 = n (Option.value_exn s1)
      ; s2 = n (Option.value_exn s2)
      ; s3 = n (Option.value_exn s3)
      ; s4 = n (Option.value_exn s4)
      ; s5 = n (Option.value_exn s5)
      ; s6 = n (Option.value_exn s6)
      ; s7 = n (Option.value_exn s7)
      ; s8 = n (Option.value_exn s8)
      ; s9 = n (Option.value_exn s9)
      ; singles = n (Option.value_exn singles)
      ; triples = n (Option.value_exn triples)
      })
;;

let monograms = Incr.map incr ~f:(fun v -> n v.singles)
let bigrams = Incr.map incr ~f:(fun v -> n v.s1)

let skipgrams =
  Incr.map incr ~f:(fun v ->
      let acc = String.Table.create () in
      [ v.s2; v.s3; v.s4; v.s5; v.s6; v.s7; v.s8 ]
      |> List.fold ~init:2 ~f:(fun denom s ->
             String.Table.merge_into ~src:s ~dst:acc ~f:(fun ~key:_ a maybe_b ->
                 let res =
                   (a /. Float.of_int denom) +. Option.value maybe_b ~default:0.
                 in
                 Hashtbl.Merge_into_action.Set_to res);
             denom + 1)
      |> ignore;
      acc)
;;

let allgrams =
  Incr.map2 bigrams skipgrams ~f:(fun bigrams skipgrams ->
      Hashtbl.merge bigrams skipgrams ~f:(fun ~key:_ -> function
        | `Left a -> Some a
        | `Right b -> Some b
        | `Both (a, b) -> Some (a +. b)))
;;
