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

  let empty =
    { s1 = None
    ; s2 = None
    ; s3 = None
    ; s4 = None
    ; s5 = None
    ; s6 = None
    ; s7 = None
    ; s8 = None
    ; s9 = None
    ; singles = None
    ; triples = None
    }
  ;;

  let incr =
    let json =
      let path =
        match Sites.Sites.corpus with
        | [ path ] -> path ^/ "data.json"
        | _ -> failwith "No path to corpus"
      in
      Jsonaf.of_string (In_channel.read_all path)
    in
    let v =
      let assoc = Jsonaf.assoc_list_exn json in
      List.fold assoc ~init:empty ~f:(fun acc (key, json) ->
          if String.is_suffix key ~suffix:"-skipgram"
          then (
            let s =
              Jsonaf.assoc_list_exn json
              |> List.Assoc.map ~f:Jsonaf.float_exn
              |> String.Table.of_alist_exn
            in
            match key.[0] with
            | '1' -> { acc with s1 = Some s }
            | '2' -> { acc with s2 = Some s }
            | '3' -> { acc with s3 = Some s }
            | '4' -> { acc with s4 = Some s }
            | '5' -> { acc with s5 = Some s }
            | '6' -> { acc with s6 = Some s }
            | '7' -> { acc with s7 = Some s }
            | '8' -> { acc with s8 = Some s }
            | '9' -> { acc with s9 = Some s }
            | _ -> failwithf "Invalid field %s" key ())
          else if String.equal key "singles"
          then (
            let s =
              Jsonaf.assoc_list_exn json
              |> List.Assoc.map ~f:Jsonaf.float_exn
              |> List.map ~f:(fun (k, v) -> k.[0], v)
              |> Char.Table.of_alist_exn
            in
            { acc with singles = Some s })
          else if String.equal key "triples"
          then (
            let s =
              Jsonaf.assoc_list_exn json
              |> List.Assoc.map ~f:Jsonaf.float_exn
              |> String.Table.of_alist_exn
            in
            { acc with triples = Some s })
          else acc)
    in
    Incr.return v
  ;;
end

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
