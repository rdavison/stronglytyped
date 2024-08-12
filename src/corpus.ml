open! Import

type 'a info =
  { s1 : 'a String.Table.t * 'a
  ; s2 : 'a String.Table.t * 'a
  ; s3 : 'a String.Table.t * 'a
  ; s4 : 'a String.Table.t * 'a
  ; s5 : 'a String.Table.t * 'a
  ; s6 : 'a String.Table.t * 'a
  ; s7 : 'a String.Table.t * 'a
  ; s8 : 'a String.Table.t * 'a
  ; s9 : 'a String.Table.t * 'a
  ; singles : 'a Char.Table.t * 'a
  ; triples : 'a String.Table.t * 'a
  ; words : 'a String.Table.t * 'a
  }
[@@deriving sexp]

type t =
  { freqs : float info
  ; counts : Bignum.t info
  }
[@@deriving sexp]

let load_corpus name =
  let data =
    In_channel.read_all name
    (* (match Sites.Sites.corpus with
       | [ path ] -> path ^/ name
       | _ -> failwith "No path to corpus") *)
  in
  let sexp = Sexp.of_string data in
  t_of_sexp sexp
;;

let n tbl =
  let total = List.sum (module Bignum) (Hashtbl.data tbl) ~f:Fn.id in
  ( Hashtbl.map tbl ~f:(fun v -> Bignum.(v / total) |> Bignum.to_float)
  , Bignum.to_float total )
;;

let c tbl =
  let total = List.sum (module Bignum) (Hashtbl.data tbl) ~f:Fn.id in
  tbl, total
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
    let c1 = s.[i] in
    Hashtbl.update singles c1 ~f:(function
      | Some v -> Bignum.(v + one)
      | None -> Bignum.one);
    for j = 0 to sn_len - 1 do
      if i + j + 1 < len
      then (
        let c2 = s.[i + j + 1] in
        if i + j + 2 < len
        then (
          let c3 = s.[i + j + 2] in
          Hashtbl.update
            triples
            (String.of_char_list [ c1; c2; c3 ])
            ~f:(function
              | Some v -> Bignum.(v + one)
              | None -> Bignum.one);
          Hashtbl.update
            sn.(j)
            (String.of_char_list [ c1; c2 ])
            ~f:(function
              | Some v -> Bignum.(v + one)
              | None -> Bignum.one)))
    done
  done;
  let words_table = String.Table.create () in
  let words = String.split_on_chars s ~on:[ ' '; '\n'; '\r'; '\t' ] in
  List.iter words ~f:(fun word ->
    Hashtbl.update words_table word ~f:(function
      | None -> Bignum.one
      | Some count -> Bignum.(count + one)));
  let freqs =
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
    ; words = n words_table
    }
  in
  let counts =
    { s1 = c s1
    ; s2 = c s2
    ; s3 = c s3
    ; s4 = c s4
    ; s5 = c s5
    ; s6 = c s6
    ; s7 = c s7
    ; s8 = c s8
    ; s9 = c s9
    ; singles = c singles
    ; triples = c triples
    ; words = c words_table
    }
  in
  let res = { freqs; counts } in
  let same =
    let s = sexp_of_t res |> Sexp.to_string_mach in
    let v = Sexp.of_string s |> t_of_sexp in
    let s' = sexp_of_t v |> Sexp.to_string_mach in
    String.equal s s'
  in
  if not same then failwith "not same";
  res
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
