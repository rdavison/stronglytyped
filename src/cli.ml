open! Import

module Worker = struct
  type t =
    { pretty_stats : string
    ; pretty_score : string
    ; pretty_layout : string
    ; final_score : float
    ; save_state : Layout.save_state
    }
  [@@deriving sexp]

  let to_string { pretty_stats; pretty_score; pretty_layout; final_score; save_state = _ }
    =
    sprintf
      "STATS\n\
       ==========\n\
       %s\n\n\
       SCORE\n\
       ==========\n\
       %s\n\n\
       FINAL SCORE\n\
       ==========\n\
       %.12f\n\n\
       LAYOUT\n\
       ==========\n\
       %s"
      pretty_stats
      pretty_score
      final_score
      pretty_layout
  ;;

  let run ~corpus =
    let module Incr = Incremental.Make () in
    let module Layout = Layout.Make (Incr) in
    let module Stats = Stats.Make (Incr) (Layout) in
    let module Score = Score.Make (Incr) (Layout) (Stats) in
    let module Gen = Gen.Make (Incr) (Layout) (Stats) (Score) in
    let corpus = Corpus.load_corpus corpus in
    let layout = Layout.ansi () in
    Layout.scramble layout 30;
    let stats = Stats.make layout corpus in
    let score = Score.default_config stats in
    let final_score_obs = Incr.observe (Score.final_sum score) in
    let initial_temperature = 100.0 in
    let cooling_rate = 0.9998 in
    let num_iterations = 1_00_000 in
    (* let num_iterations = 0 in *)
    let gen =
      Gen.anneal
        layout
        ~final_score_obs
        ~initial_temperature
        ~cooling_rate
        ~num_iterations
    in
    Layout.load layout gen.save_state;
    Incr.stabilize ();
    let gen = Gen.bruteforce layout ~final_score_obs ~mode:`Fast in
    Layout.load layout gen.save_state;
    let pretty_stats_obs = Incr.observe (Stats.pretty_string stats) in
    Incr.stabilize ();
    { pretty_stats = Incr.Observer.value_exn pretty_stats_obs
    ; pretty_score = [%sexp_of: Score.t Incr.t] score |> Sexp.to_string_hum
    ; pretty_layout = Layout.pretty_string layout
    ; final_score = gen.final_score
    ; save_state = gen.save_state
    }
  ;;
end

let main_single ~corpus =
  let best = ref [] in
  while true do
    let res = Worker.run ~corpus in
    best := res :: !best;
    !best
    |> List.sort ~compare:(fun w1 w2 -> Float.compare w1.final_score w2.final_score)
    |> (fun l -> List.take l 10)
    |> (fun l ->
         best := l;
         !best)
    |> List.rev
    |> List.iter ~f:(Fn.compose (printf "%s\n%!") Worker.to_string)
  done
;;

let main_par ~threads ~corpus =
  let module T = Domainslib.Task in
  let pool = T.setup_pool ~num_domains:threads () in
  let best = ref [] in
  while true do
    let res =
      T.run pool (fun () ->
        T.get_num_domains pool - 1
        |> List.init ~f:(fun _ -> T.async pool (fun () -> Worker.run ~corpus))
        |> List.map ~f:(T.await pool))
    in
    best := res @ !best;
    !best
    |> List.sort ~compare:(fun w1 w2 -> Float.compare w1.final_score w2.final_score)
    |> (fun l -> List.take l 10)
    |> (fun l ->
         best := l;
         !best)
    |> List.rev
    |> List.iter ~f:(Fn.compose (printf "%s\n%!") Worker.to_string)
  done;
  T.teardown_pool pool
;;

module Default = struct
  let threads = Domain.recommended_domain_count () - 1
end

let gen =
  let param =
    let open Command.Let_syntax in
    let%map_open corpus = anon ("corpus" %: string)
    and threads =
      flag
        "-t"
        (optional_with_default Default.threads int)
        ~doc:(sprintf "INT Number of threads to use. Default: %d" Default.threads)
    in
    fun () ->
      match threads with
      | 1 -> main_single ~corpus
      | _ -> main_par ~threads ~corpus
  in
  Command.basic ~summary:"Generate layouts" param
;;

let freqs =
  let param =
    let open Command.Let_syntax in
    let%map_open case_insensitive =
      let default = false in
      flag
        "-i"
        no_arg
        ~doc:(sprintf " Pass to turn on case-insensitivity. Default: %b" default)
    and corpus = anon ("corpus" %: string)
    and query = anon (non_empty_sequence_as_list ("query" %: string)) in
    fun () ->
      let corpus = Corpus.load_corpus corpus in
      let corpus =
        if case_insensitive
        then
          { Corpus.s1 =
              corpus.s1
              |> Hashtbl.to_alist
              |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
              |> String.Table.of_alist_multi
              |> Hashtbl.map ~f:(List.sum (module Float) ~f:Fn.id)
          ; s2 =
              corpus.s2
              |> Hashtbl.to_alist
              |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
              |> String.Table.of_alist_multi
              |> Hashtbl.map ~f:(List.sum (module Float) ~f:Fn.id)
          ; s3 =
              corpus.s3
              |> Hashtbl.to_alist
              |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
              |> String.Table.of_alist_multi
              |> Hashtbl.map ~f:(List.sum (module Float) ~f:Fn.id)
          ; s4 =
              corpus.s4
              |> Hashtbl.to_alist
              |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
              |> String.Table.of_alist_multi
              |> Hashtbl.map ~f:(List.sum (module Float) ~f:Fn.id)
          ; s5 =
              corpus.s5
              |> Hashtbl.to_alist
              |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
              |> String.Table.of_alist_multi
              |> Hashtbl.map ~f:(List.sum (module Float) ~f:Fn.id)
          ; s6 =
              corpus.s6
              |> Hashtbl.to_alist
              |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
              |> String.Table.of_alist_multi
              |> Hashtbl.map ~f:(List.sum (module Float) ~f:Fn.id)
          ; s7 =
              corpus.s7
              |> Hashtbl.to_alist
              |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
              |> String.Table.of_alist_multi
              |> Hashtbl.map ~f:(List.sum (module Float) ~f:Fn.id)
          ; s8 =
              corpus.s8
              |> Hashtbl.to_alist
              |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
              |> String.Table.of_alist_multi
              |> Hashtbl.map ~f:(List.sum (module Float) ~f:Fn.id)
          ; s9 =
              corpus.s9
              |> Hashtbl.to_alist
              |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
              |> String.Table.of_alist_multi
              |> Hashtbl.map ~f:(List.sum (module Float) ~f:Fn.id)
          ; singles =
              corpus.singles
              |> Hashtbl.to_alist
              |> List.map ~f:(fun (k, v) -> Char.lowercase k, v)
              |> Char.Table.of_alist_multi
              |> Hashtbl.map ~f:(List.sum (module Float) ~f:Fn.id)
          ; triples =
              corpus.triples
              |> Hashtbl.to_alist
              |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
              |> String.Table.of_alist_multi
              |> Hashtbl.map ~f:(List.sum (module Float) ~f:Fn.id)
          }
        else corpus
      in
      let fullskiptotal =
        [ corpus.s2
        ; corpus.s3
        ; corpus.s4
        ; corpus.s5
        ; corpus.s6
        ; corpus.s7
        ; corpus.s8
        ; corpus.s9
        ]
        |> List.mapi ~f:(fun i table ->
          (Hashtbl.data table |> List.sum (module Float) ~f:Fn.id) /. exp (Float.of_int i))
        |> List.sum (module Float) ~f:Fn.id
      in
      let abbas =
        List.map query ~f:(fun q ->
          let p = String.rev q in
          let abba =
            match String.to_list q with
            | [ a; '_'; b ] ->
              let ab =
                Hashtbl.find corpus.s2 (String.of_char_list [ a; b ])
                |> Option.value ~default:0.
              in
              let ba =
                Hashtbl.find corpus.s2 (String.of_char_list [ b; a ])
                |> Option.value ~default:0.
              in
              q, ab, p, ba
            | [ a; '_'; '_'; b ] ->
              let ab =
                [ Hashtbl.find corpus.s2 (String.of_char_list [ a; b ])
                ; Hashtbl.find corpus.s3 (String.of_char_list [ a; b ])
                ; Hashtbl.find corpus.s4 (String.of_char_list [ a; b ])
                ; Hashtbl.find corpus.s5 (String.of_char_list [ a; b ])
                ; Hashtbl.find corpus.s6 (String.of_char_list [ a; b ])
                ; Hashtbl.find corpus.s7 (String.of_char_list [ a; b ])
                ; Hashtbl.find corpus.s8 (String.of_char_list [ a; b ])
                ; Hashtbl.find corpus.s9 (String.of_char_list [ a; b ])
                ]
                |> List.mapi ~f:(fun i x ->
                  match x with
                  | None -> 0.
                  | Some x -> x /. exp (Float.of_int i))
                |> List.sum (module Float) ~f:Fn.id
                |> fun x -> x /. fullskiptotal
              in
              let ba =
                [ Hashtbl.find corpus.s2 (String.of_char_list [ b; a ])
                ; Hashtbl.find corpus.s3 (String.of_char_list [ b; a ])
                ; Hashtbl.find corpus.s4 (String.of_char_list [ b; a ])
                ; Hashtbl.find corpus.s5 (String.of_char_list [ b; a ])
                ; Hashtbl.find corpus.s6 (String.of_char_list [ b; a ])
                ; Hashtbl.find corpus.s7 (String.of_char_list [ b; a ])
                ; Hashtbl.find corpus.s8 (String.of_char_list [ b; a ])
                ; Hashtbl.find corpus.s9 (String.of_char_list [ b; a ])
                ]
                |> List.mapi ~f:(fun i x ->
                  match x with
                  | None -> 0.
                  | Some x -> x /. exp (Float.of_int i))
                |> List.sum (module Float) ~f:Fn.id
                |> fun x -> x /. fullskiptotal
              in
              q, ab, p, ba
            | [ a; b ] ->
              let ab =
                Hashtbl.find corpus.s2 (String.of_char_list [ a; b ])
                |> Option.value ~default:0.
              in
              let ba =
                Hashtbl.find corpus.s2 (String.of_char_list [ b; a ])
                |> Option.value ~default:0.
              in
              q, ab, p, ba
            | other ->
              failwiths ~here:[%here] "Unsupported format" other [%sexp_of: char list]
          in
          abba)
      in
      let msg =
        let body =
          String.concat
            ~sep:"\n"
            (List.map abbas ~f:(fun (p, ab, q, ba) ->
               String.concat
                 ~sep:"\n"
                 [ sprintf "%s + %s: %.2f%%" p q ((ab +. ba) *. 100.)
                 ; sprintf "  %s: %.2f%%" p (ab *. 100.)
                 ; sprintf "  %s: %.2f%%" q (ba *. 100.)
                 ]))
        in
        let footer =
          let left =
            String.concat
              ~sep:" + "
              (List.concat_map abbas ~f:(fun (p, _, q, _) -> [ p; q ]))
          in
          let right = List.sum (module Float) abbas ~f:(fun (_, ab, _, ba) -> ab +. ba) in
          sprintf "%s: %.2f%%" left (right *. 100.)
        in
        String.concat ~sep:"\n" [ body; footer ]
      in
      printf "%s\n" msg
  in
  Command.basic ~summary:"Corpus frequency analysis" param
;;

let corpus =
  let param =
    let open Command.Let_syntax in
    let%map_open text = anon ("text" %: string) in
    fun () ->
      let data = In_channel.read_all text in
      let corpus = Corpus.of_string data in
      print_s ([%sexp_of: Corpus.t] corpus)
  in
  Command.basic ~summary:"Text to corpus (Don't forget to redirect to a file)" param
;;

let cmd =
  let group = [ "gen", gen; "freqs", freqs; "corpus", corpus ] in
  Command.group ~summary:"StronglyTyped Keyboard Layout Analysis" group
;;
