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

let data_v = Incr.Var.create ""
let data = Incr.Var.watch data_v

let incr =
  let%map.Incr v = data in
  Sexp.of_string v |> t_of_sexp
;;

let monograms =
  let%map.Incr v = incr in
  v.singles
;;

let bigrams =
  let%map.Incr v = incr in
  v.s1
;;

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

let n tbl =
  let total = List.sum (module Float) (Hashtbl.data tbl) ~f:Fn.id in
  Hashtbl.map tbl ~f:(fun v -> v /. total)
;;

let of_string s =
  let len = String.length s in
  let singles = Char.Table.create () in
  let triples = String.Table.create () in
  let s1 = String.Table.create () in
  let s2 = String.Table.create () in
  let s3 = String.Table.create () in
  let s4 = String.Table.create () in
  let s5 = String.Table.create () in
  let s6 = String.Table.create () in
  let s7 = String.Table.create () in
  let s8 = String.Table.create () in
  let s9 = String.Table.create () in
  let sn = [| s1; s2; s3; s4; s5; s6; s7; s8; s9 |] in
  let sn_len = Array.length sn in
  for i = 0 to len - 1 do
    let c1 = Char.lowercase s.[i] in
    Char.Table.update singles c1 ~f:(function
        | Some v -> v +. 1.
        | None -> 1.);
    for j = 0 to sn_len - 1 do
      if i + j + 1 < len
      then (
        let c2 = Char.lowercase s.[i + j + 1] in
        if i + j + 2 < len
        then (
          let c3 = Char.lowercase s.[i + j + 2] in
          String.Table.update
            triples
            (String.of_char_list [ c1; c2; c3 ])
            ~f:(function
              | Some v -> v +. 1.
              | None -> 1.);
          String.Table.update
            sn.(j)
            (String.of_char_list [ c1; c2 ])
            ~f:(function
              | Some v -> v +. 1.
              | None -> 1.)))
    done
  done;
  let res =
    { s1 = n s1
    ; s2 = n s2
    ; s3 = n s3
    ; s4 = n s4
    ; s5 = n s5
    ; s6 = n s6
    ; s7 = n s7
    ; s8 = n s8
    ; s9 = n s9
    ; singles = n singles
    ; triples = n triples
    }
  in
  let same =
    let s = sexp_of_t res |> Sexp.to_string_mach in
    let v = Sexp.of_string s |> t_of_sexp in
    let s' = sexp_of_t v |> Sexp.to_string_mach in
    String.equal s s'
  in
  if not same then failwith "not same";
  res
;;
