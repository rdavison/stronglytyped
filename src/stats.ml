open! Import
include Stats_intf

module Make (Incr : Incremental.S) (Layout : Layout.S with module Incr = Incr) :
  S with module Incr = Incr and module Layout = Layout = struct
  module Incr = Incr
  module Layout = Layout

  type t =
    { usage : float Incr.t Hand_finger.Map.t (* finger usage*)
    ; sfb : float Incr.t Hand_finger.Map.t (* same finger bigram *)
    ; shb : float Incr.t Hand.Map.t (* same hand bigram *)
    ; sfs : float Incr.t Hand_finger.Map.t (* same finger skipgram *)
    ; shs : float Incr.t Hand.Map.t (* same hand skipgram *)
    ; speed : float Incr.t Hand_finger.Map.t (* finger speed *)
    ; fsb : float Incr.t Hand_finger2.Map.t (* full scissor bigram *)
    ; hsb : float Incr.t Hand_finger2.Map.t (* half scissor bigram *)
    ; fss : float Incr.t Hand_finger2.Map.t (* full scissor skipgram *)
    ; hss : float Incr.t Hand_finger2.Map.t (* half scissor skipgram *)
    ; lsb : float Incr.t Hand_finger2.Map.t (* lateral stretch bigram *)
    ; lss : float Incr.t Hand_finger2.Map.t (* lateral scretch skipgram *)
    ; srb : float Incr.t Hand_finger2.Map.t (* same row adjacent finger bigram *)
    }
  [@@deriving sexp_of]

  let s29 = Hashtbl.create (module String)

  (* let finger_dist f1 f2 =
     match f1, f2 with
     | `P, `R | `R, `P -> Some 1.
     | `P, `M | `M, `P -> Some 2.
     | `P, `I | `I, `P -> Some 3.
     | `R, `M | `M, `R -> Some 1.
     | `R, `I | `I, `R -> Some 2.
     | `M, `I | `I, `M -> Some 1.
     | (_ : Finger.t), (_ : Finger.t) -> None
     ;; *)

  let make (layout_ : Layout.t) (corpus : Corpus.t) =
    let module M = struct
      type t =
        { usage : float Incr.t list Hand_finger.Table.t
        ; sfb : float Incr.t list Hand_finger.Table.t
        ; shb : float Incr.t list Hand.Table.t
        ; sfs : float Incr.t list Hand_finger.Table.t
        ; shs : float Incr.t list Hand.Table.t
        ; speed : float Incr.t list Hand_finger.Table.t
        ; fsb : float Incr.t list Hand_finger2.Table.t
        ; hsb : float Incr.t list Hand_finger2.Table.t
        ; fss : float Incr.t list Hand_finger2.Table.t
        ; hss : float Incr.t list Hand_finger2.Table.t
        ; lsb : float Incr.t list Hand_finger2.Table.t
        ; lss : float Incr.t list Hand_finger2.Table.t
        ; srb : float Incr.t list Hand_finger2.Table.t
        }

      let empty () =
        { usage = Hand_finger.Table.create ()
        ; sfb = Hand_finger.Table.create ()
        ; shb = Hand.Table.create ()
        ; sfs = Hand_finger.Table.create ()
        ; shs = Hand.Table.create ()
        ; speed = Hand_finger.Table.create ()
        ; fsb = Hand_finger2.Table.create ()
        ; hsb = Hand_finger2.Table.create ()
        ; fss = Hand_finger2.Table.create ()
        ; hss = Hand_finger2.Table.create ()
        ; lsb = Hand_finger2.Table.create ()
        ; lss = Hand_finger2.Table.create ()
        ; srb = Hand_finger2.Table.create ()
        }
      ;;

      let v =
        let layout =
          Array.map layout_.keys ~f:(fun (key, var) -> key, Incr.Var.watch var)
        in
        let acc = empty () in
        Array.iteri layout ~f:(fun i (k1, var1) ->
          let _l1, o1 = Layout.layer_offset layout_ i in
          let ((h1, f1) as hf1) = Key.hand_finger k1 in
          let monogram = Incr.map var1 ~f:(fun v1 -> v1.code) in
          let freq_mono =
            Incr.map monogram ~f:(Corpus.Lookup.freq1 ~data:corpus.singles)
          in
          let (* usage *) () =
            Hashtbl.update acc.usage hf1 ~f:(function
              | None -> [ freq_mono ]
              | Some freq -> freq_mono :: freq)
          in
          (* bigram stats *)
          Array.iteri layout ~f:(fun j (k2, var2) ->
            let _l2, o2 = Layout.layer_offset layout_ j in
            let h2, f2 = Key.hand_finger k2 in
            let dx = k2.x -. k1.x in
            let dy = k2.y -. k2.y in
            let dist = Float.sqrt ((dy ** 2.) +. (dx ** 2.)) in
            let bigram =
              Incr.both var1 var2 |> Incr.map ~f:(fun (v1, v2) -> v1.code, v2.code)
            in
            let freq_s1 = Incr.map bigram ~f:(Corpus.Lookup.freq2 ~data:corpus.s1) in
            let freq_s29 =
              Incr.map bigram ~f:(fun ((c1, c2) as bigram) ->
                let bigram' = Code.to_string c1 ^ Code.to_string c2 in
                Hashtbl.find_or_add s29 bigram' ~default:(fun () ->
                  [ Corpus.Lookup.freq2 bigram ~data:corpus.s2
                  ; Corpus.Lookup.freq2 bigram ~data:corpus.s3
                  ; Corpus.Lookup.freq2 bigram ~data:corpus.s4
                  ; Corpus.Lookup.freq2 bigram ~data:corpus.s5
                  ; Corpus.Lookup.freq2 bigram ~data:corpus.s6
                  ; Corpus.Lookup.freq2 bigram ~data:corpus.s7
                  ; Corpus.Lookup.freq2 bigram ~data:corpus.s8
                  ; Corpus.Lookup.freq2 bigram ~data:corpus.s9
                  ]
                  |> List.foldi ~init:0. ~f:(fun i acc sn ->
                    acc +. (sn /. Float.exp (Float.of_int (i + 1))))))
            in
            let freq_s129 = Incr.map2 freq_s1 freq_s29 ~f:(fun s1 s29 -> s1 +. s29) in
            if Hand.Infix.(h1 = h2)
            then (
              (* shb *)
              Hashtbl.update acc.shb h1 ~f:(function
                | None -> [ freq_s1 ]
                | Some freqs -> freq_s1 :: freqs);
              (* shs *)
              Hashtbl.update acc.shs h1 ~f:(function
                | None -> [ freq_s29 ]
                | Some freqs -> freq_s29 :: freqs);
              (* same finger *)
              if o1 = o2
              then ()
              else if Finger.Infix.(f1 = f2)
              then (
                (* sfb *)
                Hashtbl.update acc.sfb hf1 ~f:(function
                  | None -> [ freq_s1 ]
                  | Some freqs -> freq_s1 :: freqs);
                (* sfs *)
                Hashtbl.update acc.sfs hf1 ~f:(function
                  | None -> [ freq_s29 ]
                  | Some freqs -> freq_s29 :: freqs);
                (* speed *)
                let speed = Incr.map freq_s129 ~f:(fun freq_s129 -> dist *. freq_s129) in
                Hashtbl.update acc.speed hf1 ~f:(function
                  | None -> [ speed ]
                  | Some speeds -> speed :: speeds))
              else (
                match f1, f2 with
                | `T, _ | _, `T -> ()
                | _, _ ->
                  let fs () =
                    (* fsb *)
                    Hashtbl.update acc.fsb (h1, f1, f2) ~f:(function
                      | None -> [ freq_s1 ]
                      | Some fsb -> freq_s1 :: fsb);
                    (* fss *)
                    Hashtbl.update acc.fss (h1, f1, f2) ~f:(function
                      | None -> [ freq_s29 ]
                      | Some fss -> freq_s29 :: fss)
                  in
                  let hs () =
                    (* hsb *)
                    Hashtbl.update acc.hsb (h1, f1, f2) ~f:(function
                      | None -> [ freq_s1 ]
                      | Some hsb -> freq_s1 :: hsb);
                    (* hss *)
                    Hashtbl.update acc.hss (h1, f1, f2) ~f:(function
                      | None -> [ freq_s29 ]
                      | Some hss -> freq_s29 :: hss)
                  in
                  let is_ls = Finger.adjacent f1 f2 && Float.( > ) (Float.abs dx) 1.5 in
                  let is_inner (k : Key.t) = k.col = 5 || k.col = 6 in
                  let is_left =
                    match k1.hand with
                    | `L -> true
                    | `R -> false
                  in
                  (* lateral stretch *)
                  if is_ls
                  then (
                    (* lsb *)
                    Hashtbl.update acc.lsb (h1, f1, f2) ~f:(function
                      | None -> [ freq_s1 ]
                      | Some lsb -> freq_s1 :: lsb);
                    (* lss *)
                    Hashtbl.update acc.lss (h1, f1, f2) ~f:(function
                      | None -> [ freq_s29 ]
                      | Some lss -> freq_s29 :: lss));
                  (* scissors *)
                  (match f1, f2 with
                   | `P, `P | `R, `R | `M, `M | `I, `I | `T, _ | _, `T -> ()
                   | `P, `R ->
                     (match k1.row - k2.row with
                      | -2 -> fs ()
                      | -1 -> ()
                      | 0 -> ()
                      | 1 -> hs ()
                      | 2 -> fs ()
                      | _ -> ())
                   | `P, `M ->
                     (match k1.row - k2.row with
                      | -2 -> hs ()
                      | -1 -> ()
                      | 0 -> ()
                      | 1 -> hs ()
                      | 2 -> fs ()
                      | _ -> ())
                   | `P, `I ->
                     (match k1.row - k2.row with
                      | -2 -> fs ()
                      | -1 -> ()
                      | 0 -> ()
                      | 1 -> ()
                      | 2 -> ()
                      | _ -> ())
                   | `R, `P ->
                     (match k1.row - k2.row with
                      | -2 -> fs ()
                      | -1 -> hs ()
                      | 0 -> ()
                      | 1 -> ()
                      | 2 -> fs ()
                      | _ -> ())
                   | `R, `M ->
                     (match k1.row - k2.row with
                      | -2 -> fs ()
                      | -1 -> hs ()
                      | 0 -> ()
                      | 1 -> if is_left then if k2.row = 0 then fs () else hs () else ()
                      | 2 -> if is_left then if k2.row = 0 then fs () else hs () else ()
                      | _ -> ())
                   | `R, `I ->
                     (match k1.row - k2.row with
                      | -2 -> fs ()
                      | -1 -> (if is_inner k2 then fs else hs) ()
                      | 0 -> ()
                      | 1 ->
                        if is_left
                        then
                          if is_inner k2 then if k2.row = 0 then fs () else hs () else ()
                        else ()
                      | 2 ->
                        if is_left
                        then
                          if is_inner k2
                          then if k2.row = 0 then fs () else hs ()
                          else hs ()
                        else ()
                      | _ -> ())
                   | `M, `P ->
                     (match k1.row - k2.row with
                      | -2 -> fs ()
                      | -1 -> hs ()
                      | 0 -> ()
                      | 1 -> ()
                      | 2 -> hs ()
                      | _ -> ())
                   | `M, `R ->
                     (match k1.row - k2.row with
                      | -2 -> fs ()
                      | -1 -> hs ()
                      | 0 -> ()
                      | 1 -> if is_left then if k2.row = 0 then fs () else hs () else ()
                      | 2 -> if is_left then if k2.row = 0 then fs () else hs () else ()
                      | _ -> ())
                   | `M, `I ->
                     if Hand.equal h1 `L && k1.col - k2.col > 0 then fs ();
                     (match k1.row - k2.row with
                      | -2 -> fs ()
                      | -1 ->
                        (match h1 with
                         | `L -> (if is_ls then fs else hs) ()
                         | `R -> if is_ls then fs ())
                      | 0 -> ()
                      | 1 -> if Hand.equal h1 `L then if is_ls then hs ()
                      | 2 -> if Hand.equal h1 `L then (if is_ls then fs else hs) ()
                      | _ -> ())
                   | `I, `P ->
                     (match k1.row - k2.row with
                      | -2 -> ()
                      | -1 -> ()
                      | 0 -> ()
                      | 1 -> hs ()
                      | 2 -> fs ()
                      | _ -> ())
                   | `I, `R ->
                     (match k1.row - k2.row with
                      | -2 ->
                        if is_left
                        then
                          if is_inner k1 then if k1.row = 0 then fs () else hs () else ()
                        else ()
                      | -1 ->
                        if is_left
                        then
                          if is_inner k1 then if k1.row = 0 then fs () else hs () else ()
                        else ()
                      | 0 -> ()
                      | 1 -> (if is_inner k1 then fs else hs) ()
                      | 2 -> fs ()
                      | _ -> ())
                   | `I, `M ->
                     if Hand.equal h1 `L && k1.col - k2.col < 0 then fs ();
                     (match k1.row - k2.row with
                      | -2 -> if Hand.equal h1 `L then (if is_ls then fs else hs) ()
                      | -1 -> if Hand.equal h1 `L then if is_ls then hs ()
                      | 0 -> ()
                      | 1 ->
                        (match h1 with
                         | `L -> (if is_ls then fs else hs) ()
                         | `R -> if is_ls then fs ())
                      | 2 -> fs ()
                      | _ -> ()));
                  (* srb*)
                  if k1.row = k2.row && not is_ls
                  then
                    Hashtbl.update acc.srb (h1, f1, f2) ~f:(function
                      | None -> [ freq_s1 ]
                      | Some srb -> freq_s1 :: srb)))));
        acc
      ;;
    end
    in
    let f of_alist_exn data =
      of_alist_exn (Hashtbl.to_alist data)
      |> Map.map ~f:(fun lst -> Array.of_list lst |> Incr.sum_float)
    in
    let hand_finger = f Hand_finger.Map.of_alist_exn in
    let hand_finger2 = f Hand_finger2.Map.of_alist_exn in
    let hand = f Hand.Map.of_alist_exn in
    { usage = hand_finger M.v.usage
    ; sfb = hand_finger M.v.sfb
    ; shb = hand M.v.shb
    ; sfs = hand_finger M.v.sfs
    ; shs = hand M.v.shs
    ; speed = hand_finger M.v.speed
    ; fsb = hand_finger2 M.v.fsb
    ; hsb = hand_finger2 M.v.hsb
    ; fss = hand_finger2 M.v.fss
    ; hss = hand_finger2 M.v.hss
    ; lsb = hand_finger2 M.v.lsb
    ; lss = hand_finger2 M.v.lss
    ; srb = hand_finger2 M.v.srb
    }
  ;;

  let pretty_string
    { usage; sfb; shb; sfs; shs; speed; fsb; hsb; fss; hss; lsb; lss; srb }
    =
    let map stat of_alist_exn =
      stat
      |> Map.to_alist
      |> List.map ~f:(fun (k, incr) -> Incr.map incr ~f:(fun v -> k, v))
      |> Incr.all
      |> Incr.map ~f:of_alist_exn
    in
    let%map_open.Incr usage = map usage Hand_finger.Map.of_alist_exn
    and sfb = map sfb Hand_finger.Map.of_alist_exn
    and shb = map shb Hand.Map.of_alist_exn
    and sfs = map sfs Hand_finger.Map.of_alist_exn
    and shs = map shs Hand.Map.of_alist_exn
    and speed = map speed Hand_finger.Map.of_alist_exn
    and fsb = map fsb Hand_finger2.Map.of_alist_exn
    and hsb = map hsb Hand_finger2.Map.of_alist_exn
    and fss = map fss Hand_finger2.Map.of_alist_exn
    and hss = map hss Hand_finger2.Map.of_alist_exn
    and lsb = map lsb Hand_finger2.Map.of_alist_exn
    and lss = map lss Hand_finger2.Map.of_alist_exn
    and srb = map srb Hand_finger2.Map.of_alist_exn in
    let module T = Text_block in
    let table data ~to_string ~all =
      let cols =
        all
        |> List.map ~f:(fun key ->
          let data =
            data
            |> List.map ~f:snd
            |> List.map ~f:(fun map ->
              match Map.find (fst map) key with
              | None -> T.text "None"
              | Some item -> item |> Float.( * ) 100. |> sprintf "%.2f" |> T.text)
          in
          T.text (to_string key) :: data, `Right)
        |> List.cons
             ( T.text "(total)"
               :: List.map data ~f:(fun x ->
                 x |> snd |> snd |> Float.( * ) 100. |> sprintf "%.2f" |> T.text)
             , `Right )
        |> List.cons (T.nil :: List.map data ~f:(Fn.compose T.text fst), `Right)
      in
      let (`Rows rows) = T.table (`Cols cols) in
      T.vcat rows
    in
    let _simple data =
      let data =
        List.map data ~f:(fun (name, total) -> name, (String.Map.empty, total))
      in
      table data ~to_string:Fn.id ~all:[]
    in
    let map_total map = Map.data map |> List.sum (module Float) ~f:Fn.id in
    let map_row name map = name, (map, map_total map) in
    let hf_table =
      table
        ~all:Hand_finger.all
        ~to_string:Hand_finger.to_string
        [ map_row "sfb" sfb
        ; map_row "sfs" sfs
        ; map_row "speed" speed
        ; map_row "usage" usage
        ]
    in
    let hand_table =
      table
        ~all:Hand.all
        ~to_string:Hand.to_string
        [ map_row "shb" shb; map_row "shs" shs ]
    in
    let hf2_table =
      table
        ~all:Hand_finger2.all
        ~to_string:Hand_finger2.to_string
        [ map_row "fsb" fsb
        ; map_row "hsb" hsb
        ; map_row "fss" fss
        ; map_row "hss" hss
        ; map_row "lsb" lsb
        ; map_row "lss" lss
        ; map_row "srb" srb
        ]
    in
    let sections =
      [ "hand-finger", hf_table; "hand", hand_table; "hand-finger2", hf2_table ]
    in
    let t =
      sections
      |> List.map ~f:(fun (title, table) ->
        let title = title |> T.text in
        let table = table |> T.Boxed.cell |> T.boxed in
        T.vcat [ title; table ])
      |> T.vcat
    in
    T.render t
  ;;
end
