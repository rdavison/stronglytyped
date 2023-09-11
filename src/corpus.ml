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

let merge =
  let m s1 s2 =
    Hashtbl.merge s1 s2 ~f:(fun ~key:_ data ->
      Some
        (match data with
         | `Both (a, b) -> a +. b
         | `Left a | `Right a -> a))
  in
  fun t1 t2 ->
    { s1 = m t1.s1 t2.s1
    ; s2 = m t1.s2 t2.s2
    ; s3 = m t1.s3 t2.s3
    ; s4 = m t1.s4 t2.s4
    ; s5 = m t1.s5 t2.s5
    ; s6 = m t1.s6 t2.s6
    ; s7 = m t1.s7 t2.s7
    ; s8 = m t1.s8 t2.s8
    ; s9 = m t1.s9 t2.s9
    ; singles = m t1.singles t2.singles
    ; triples = m t1.triples t2.triples
    }
;;

let empty () =
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
  { s1; s2; s3; s4; s5; s6; s7; s8; s9; singles; triples }
;;

let n tbl =
  let total = List.sum (module Float) (Hashtbl.data tbl) ~f:Fn.id in
  Hashtbl.map tbl ~f:(fun v -> v /. total)
;;

let of_string s =
  let len = String.length s in
  let worker p q =
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
    let lowercase = Char.lowercase in
    for i = p to q - 1 do
      if i mod 1000000 = 0
      then
        printf
          "%d/%d = %f\n%!"
          (i - p)
          (q - p - 1)
          (Float.of_int (i - p) /. Float.of_int (q - p - 1) *. 100.);
      let c1 = lowercase s.[i] in
      Hashtbl.update singles c1 ~f:(function
        | Some v -> v +. 1.
        | None -> 1.);
      for j = 0 to sn_len - 1 do
        if i + j + 1 < len
        then (
          let c2 = lowercase s.[i + j + 1] in
          if i + j + 2 < len
          then (
            let c3 = lowercase s.[i + j + 2] in
            Hashtbl.update
              triples
              (String.of_char_list [ c1; c2; c3 ])
              ~f:(function
                | Some v -> v +. 1.
                | None -> 1.);
            Hashtbl.update
              sn.(j)
              (String.of_char_list [ c1; c2 ])
              ~f:(function
                | Some v -> v +. 1.
                | None -> 1.)))
      done
    done;
    { s1; s2; s3; s4; s5; s6; s7; s8; s9; singles; triples }
  in
  let num_domains = Domain.recommended_domain_count () - 1 in
  let width = len / num_domains in
  let module T = Domainslib.Task in
  let pool = T.setup_pool ~num_domains () in
  let res =
    T.run pool (fun () ->
      Domainslib.Task.parallel_for_reduce
        ~start:0
        ~finish:num_domains
        ~body:(fun domain ->
          let p = domain * width in
          let q = Int.min (p + width) len in
          worker p q)
        pool
        merge
        (empty ()))
  in
  let res =
    { s1 = n res.s1
    ; s2 = n res.s2
    ; s3 = n res.s3
    ; s4 = n res.s4
    ; s5 = n res.s5
    ; s6 = n res.s6
    ; s7 = n res.s7
    ; s8 = n res.s8
    ; s9 = n res.s9
    ; singles = n res.singles
    ; triples = n res.triples
    }
  in
  let same =
    let s = sexp_of_t res |> Sexp.to_string_mach in
    let v = Sexp.of_string s |> t_of_sexp in
    let s' = sexp_of_t v |> Sexp.to_string_mach in
    String.equal s s'
  in
  if not same then failwith "not same";
  T.teardown_pool pool;
  res
;;

let load_corpus name =
  let data =
    In_channel.read_all name
    (* (match Sites.Sites.corpus with
       | [ path ] -> path ^/ name
       | _ -> failwith "No path to corpus") *)
  in
  if not (String.is_suffix name ~suffix:".sexp")
  then (
    let res = of_string data in
    let () = sexp_of_t res |> Sexp.save (name ^ ".sexp") in
    res)
  else (
    let sexp = Sexp.of_string data in
    t_of_sexp sexp)
;;

module Lookup = struct
  let freq1 c1 ~data =
    match c1 with
    | `Char c1 -> Hashtbl.find_or_add data c1 ~default:(fun () -> 0.)
  ;;

  let freq2 (c1, c2) ~data =
    match c1, c2 with
    | `Char c1, `Char c2 ->
      Hashtbl.find_or_add data (String.of_char_list [ c1; c2 ]) ~default:(fun () -> 0.)
  ;;

  let freq3 (c1, c2, c3) ~data =
    match c1, c2, c3 with
    | `Char c1, `Char c2, `Char c3 ->
      Hashtbl.find_or_add
        data
        (String.of_char_list [ c1; c2; c3 ])
        ~default:(fun () -> 0.)
  ;;
end
