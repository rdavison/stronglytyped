open! Import

module Worker = struct
  type t =
    { pretty_stats : string
    ; pretty_score : string
    ; pretty_layout : string
    ; final_score : Score.t
    ; save_state : Layout.save_state
    }
  [@@deriving sexp]

  let to_string
    { pretty_stats; pretty_score; pretty_layout; final_score; save_state = _ }
    ~score_scalarize
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
      (score_scalarize final_score)
      pretty_layout
  ;;

  let run ~corpus ~initial_temperature ~cooling_rate ~num_iterations =
    let module Incr = Incremental.Make () in
    let module Layout = Layout.Make (Incr) in
    let module Stats = Stats.Make (Incr) (Layout) in
    let module Score = Score.Make (Incr) (Layout) (Stats) in
    let module Gen = Gen.Make (Incr) (Layout) (Stats) (Score) in
    let corpus = Corpus.load_corpus corpus in
    let layout =
      Layout.simple30
        ~stagger:true
        ~layout_str:"qwertyuiopasdfghjkl;zxcvbnm,./QWERTYUIOPASDFGHJKL:ZXCVBNM<>?"
        ~fingermap:Fingermap.Standard
    in
    Layout.scramble layout 30;
    let stats = Stats.make layout corpus in
    let score = Score.default_config stats in
    let score_obs = Incr.observe score in
    let score_compare = Score.compare_pareto Score.Info.unweighted in
    let score_scalarize = Score.scalarize Score.Info.unweighted in
    let gen =
      Gen.anneal
        layout
        ~score_obs
        ~score_compare
        ~score_scalarize
        ~initial_temperature
        ~cooling_rate
        ~num_iterations
    in
    Layout.load layout gen.save_state;
    Incr.stabilize ();
    let gen =
      Gen.bruteforce layout ~score_obs ~score_compare ~score_scalarize ~mode:`Fast
    in
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

let main_single
  ~corpus
  ~initial_temperature
  ~cooling_rate
  ~num_iterations
  ~score_compare
  ~score_scalarize
  =
  let best = ref [] in
  while true do
    let res = Worker.run ~corpus ~initial_temperature ~cooling_rate ~num_iterations in
    best := res :: !best;
    !best
    |> List.sort ~compare:(fun w1 w2 -> score_compare w1.final_score w2.final_score)
    |> (fun l -> List.take l 10)
    |> (fun l ->
         best := l;
         !best)
    |> List.rev
    |> List.iter ~f:(Fn.compose (printf "%s\n%!") (Worker.to_string ~score_scalarize))
  done
;;

let main_par
  ~threads
  ~corpus
  ~initial_temperature
  ~cooling_rate
  ~num_iterations
  ~score_compare
  ~score_scalarize
  =
  let module T = Domainslib.Task in
  let pool = T.setup_pool ~num_domains:threads () in
  let best = ref [] in
  while true do
    let res =
      T.run pool (fun () ->
        T.get_num_domains pool - 1
        |> List.init ~f:(fun _ ->
          T.async pool (fun () ->
            Worker.run ~corpus ~initial_temperature ~cooling_rate ~num_iterations))
        |> List.map ~f:(T.await pool))
    in
    best := res @ !best;
    !best
    |> List.sort ~compare:(fun w1 w2 -> score_compare w1.final_score w2.final_score)
    |> (fun l -> List.take l 10)
    |> (fun l ->
         best := l;
         !best)
    |> List.rev
    |> List.iter ~f:(Fn.compose (printf "%s\n%!") (Worker.to_string ~score_scalarize))
  done;
  T.teardown_pool pool
;;

module Default = struct
  let threads = Domain.recommended_domain_count () - 1
  let initial_temperature = 100.0
  let cooling_rate = 0.9998
  let num_iterations = 1_00_000
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
    and initial_temperature =
      flag
        "-T"
        (optional_with_default Default.initial_temperature float)
        ~doc:
          (sprintf
             "FLOAT Simulated Annealing initial temperature. Default: %f"
             Default.initial_temperature)
    and cooling_rate =
      flag
        "-C"
        (optional_with_default Default.cooling_rate float)
        ~doc:
          (sprintf
             "FLOAT Simulated Annealing rate of cooling. Default: %f"
             Default.cooling_rate)
    and num_iterations =
      flag
        "-I"
        (optional_with_default Default.num_iterations int)
        ~doc:
          (sprintf
             "INT Simulated Annealing number of iterations. Default: %d"
             Default.num_iterations)
    and unweighted = flag "-U" no_arg ~doc:" Use unweighted objective functions." in
    fun () ->
      let how = if unweighted then Score.Info.unweighted else Score.Info.weighted in
      let score_compare = Score.compare_pareto how in
      let score_scalarize = Score.scalarize how in
      match threads with
      | 1 ->
        main_single
          ~corpus
          ~initial_temperature
          ~cooling_rate
          ~num_iterations
          ~score_compare
          ~score_scalarize
      | _ ->
        main_par
          ~threads
          ~corpus
          ~initial_temperature
          ~cooling_rate
          ~num_iterations
          ~score_compare
          ~score_scalarize
  in
  Command.basic ~summary:"Generate layouts" param
;;

let analyze ~corpus ~stagger ~layout_str ~fingermap ~score_scalarize =
  let module Incr = Incremental.Make () in
  let module Layout = Layout.Make (Incr) in
  let module Stats = Stats.Make (Incr) (Layout) in
  let module Score = Score.Make (Incr) (Layout) (Stats) in
  let module Gen = Gen.Make (Incr) (Layout) (Stats) (Score) in
  let corpus = Corpus.load_corpus corpus in
  let layout = Layout.simple30 ~stagger ~layout_str ~fingermap in
  let stats = Stats.make layout corpus in
  let score = Score.default_config stats in
  let final_score_obs = Incr.observe score in
  let pretty_stats_obs = Incr.observe (Stats.pretty_string stats) in
  Incr.stabilize ();
  let save_state = Layout.save layout in
  let pretty_stats = Incr.Observer.value_exn pretty_stats_obs in
  let pretty_score = [%sexp_of: Score.t Incr.t] score |> Sexp.to_string_hum in
  let pretty_layout = Layout.pretty_string layout in
  let final_score = Incr.Observer.value_exn final_score_obs in
  let final_result =
    Worker.to_string
      { Worker.pretty_stats; pretty_score; pretty_layout; final_score; save_state }
      ~score_scalarize
  in
  printf "%s" final_result
;;

let analyze =
  let param =
    let open Command.Let_syntax in
    let%map_open corpus = anon ("corpus" %: string)
    and stagger = flag "-s" no_arg ~doc:(sprintf " Enable row stagger")
    and fingermap =
      flag
        "-m"
        (optional_with_default Fingermap.Standard Fingermap.Command.arg_type)
        ~doc:
          (sprintf
             "FINGERMAP Desired fingermap. Possible values: %s. Default: %s"
             (String.concat Fingermap.Command.possible_values ~sep:", ")
             (Fingermap.to_string Fingermap.Command.default))
    and layout_str = anon ("layout" %: string)
    and unweighted = flag "-U" no_arg ~doc:" Use unweighted objective functions." in
    fun () ->
      let how = if unweighted then Score.Info.unweighted else Score.Info.weighted in
      let score_scalarize = Score.scalarize how in
      analyze ~corpus ~stagger ~layout_str ~fingermap ~score_scalarize
  in
  Command.basic ~summary:"Analyze layout" param
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
        then (
          let fix_info
            (type a)
            (m : (module Base__Container_intf.Summable with type t = a))
            (corpus : a Corpus.info)
            =
            { Corpus.s1 =
                ( fst corpus.s1
                  |> Hashtbl.to_alist
                  |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
                  |> String.Table.of_alist_multi
                  |> Hashtbl.map ~f:(List.sum m ~f:Fn.id)
                , snd corpus.s1 )
            ; s2 =
                ( fst corpus.s2
                  |> Hashtbl.to_alist
                  |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
                  |> String.Table.of_alist_multi
                  |> Hashtbl.map ~f:(List.sum m ~f:Fn.id)
                , snd corpus.s2 )
            ; s3 =
                ( fst corpus.s3
                  |> Hashtbl.to_alist
                  |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
                  |> String.Table.of_alist_multi
                  |> Hashtbl.map ~f:(List.sum m ~f:Fn.id)
                , snd corpus.s3 )
            ; s4 =
                ( fst corpus.s4
                  |> Hashtbl.to_alist
                  |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
                  |> String.Table.of_alist_multi
                  |> Hashtbl.map ~f:(List.sum m ~f:Fn.id)
                , snd corpus.s4 )
            ; s5 =
                ( fst corpus.s5
                  |> Hashtbl.to_alist
                  |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
                  |> String.Table.of_alist_multi
                  |> Hashtbl.map ~f:(List.sum m ~f:Fn.id)
                , snd corpus.s5 )
            ; s6 =
                ( fst corpus.s6
                  |> Hashtbl.to_alist
                  |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
                  |> String.Table.of_alist_multi
                  |> Hashtbl.map ~f:(List.sum m ~f:Fn.id)
                , snd corpus.s6 )
            ; s7 =
                ( fst corpus.s7
                  |> Hashtbl.to_alist
                  |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
                  |> String.Table.of_alist_multi
                  |> Hashtbl.map ~f:(List.sum m ~f:Fn.id)
                , snd corpus.s7 )
            ; s8 =
                ( fst corpus.s8
                  |> Hashtbl.to_alist
                  |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
                  |> String.Table.of_alist_multi
                  |> Hashtbl.map ~f:(List.sum m ~f:Fn.id)
                , snd corpus.s8 )
            ; s9 =
                ( fst corpus.s9
                  |> Hashtbl.to_alist
                  |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
                  |> String.Table.of_alist_multi
                  |> Hashtbl.map ~f:(List.sum m ~f:Fn.id)
                , snd corpus.s9 )
            ; singles =
                ( fst corpus.singles
                  |> Hashtbl.to_alist
                  |> List.map ~f:(fun (k, v) -> Char.lowercase k, v)
                  |> Char.Table.of_alist_multi
                  |> Hashtbl.map ~f:(List.sum m ~f:Fn.id)
                , snd corpus.singles )
            ; triples =
                ( fst corpus.triples
                  |> Hashtbl.to_alist
                  |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
                  |> String.Table.of_alist_multi
                  |> Hashtbl.map ~f:(List.sum m ~f:Fn.id)
                , snd corpus.triples )
            ; words =
                ( fst corpus.words
                  |> Hashtbl.to_alist
                  |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
                  |> String.Table.of_alist_multi
                  |> Hashtbl.map ~f:(List.sum m ~f:Fn.id)
                , snd corpus.words )
            }
          in
          { Corpus.freqs = fix_info (module Float) corpus.freqs
          ; counts = fix_info (module Bignum) corpus.counts
          })
        else corpus
      in
      let corpus = corpus.freqs in
      let fullskiptotal =
        [ fst corpus.s2
        ; fst corpus.s3
        ; fst corpus.s4
        ; fst corpus.s5
        ; fst corpus.s6
        ; fst corpus.s7
        ; fst corpus.s8
        ; fst corpus.s9
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
                Hashtbl.find (fst corpus.s2) (String.of_char_list [ a; b ])
                |> Option.value ~default:0.
              in
              let ba =
                Hashtbl.find (fst corpus.s2) (String.of_char_list [ b; a ])
                |> Option.value ~default:0.
              in
              q, ab, p, ba
            | [ a; '_'; '_'; b ] ->
              let ab =
                [ Hashtbl.find (fst corpus.s2) (String.of_char_list [ a; b ])
                ; Hashtbl.find (fst corpus.s3) (String.of_char_list [ a; b ])
                ; Hashtbl.find (fst corpus.s4) (String.of_char_list [ a; b ])
                ; Hashtbl.find (fst corpus.s5) (String.of_char_list [ a; b ])
                ; Hashtbl.find (fst corpus.s6) (String.of_char_list [ a; b ])
                ; Hashtbl.find (fst corpus.s7) (String.of_char_list [ a; b ])
                ; Hashtbl.find (fst corpus.s8) (String.of_char_list [ a; b ])
                ; Hashtbl.find (fst corpus.s9) (String.of_char_list [ a; b ])
                ]
                |> List.mapi ~f:(fun i x ->
                  match x with
                  | None -> 0.
                  | Some x -> x /. exp (Float.of_int i))
                |> List.sum (module Float) ~f:Fn.id
                |> fun x -> x /. fullskiptotal
              in
              let ba =
                [ Hashtbl.find (fst corpus.s2) (String.of_char_list [ b; a ])
                ; Hashtbl.find (fst corpus.s3) (String.of_char_list [ b; a ])
                ; Hashtbl.find (fst corpus.s4) (String.of_char_list [ b; a ])
                ; Hashtbl.find (fst corpus.s5) (String.of_char_list [ b; a ])
                ; Hashtbl.find (fst corpus.s6) (String.of_char_list [ b; a ])
                ; Hashtbl.find (fst corpus.s7) (String.of_char_list [ b; a ])
                ; Hashtbl.find (fst corpus.s8) (String.of_char_list [ b; a ])
                ; Hashtbl.find (fst corpus.s9) (String.of_char_list [ b; a ])
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
                Hashtbl.find (fst corpus.s2) (String.of_char_list [ a; b ])
                |> Option.value ~default:0.
              in
              let ba =
                Hashtbl.find (fst corpus.s2) (String.of_char_list [ b; a ])
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

let num_to_take = 10000
let combine_scores = Bignum.( * )

let corpus_wordfreq =
  let main (corpus : Corpus.t) e10k () =
    let e10k = List.of_array e10k in
    let tscore_wscore_and_triples word =
      let word_len = String.length word in
      if word_len < 3
      then Bignum.zero, Bignum.zero, []
      else (
        let tscore = ref Bignum.zero in
        let triples = ref [] in
        for i = 0 to word_len - 3 do
          let j = i + 1 in
          let k = j + 1 in
          let trigram = String.of_char_list [ word.[i]; word.[j]; word.[k] ] in
          triples := trigram :: !triples;
          tscore
            := Bignum.( + )
                 !tscore
                 (Hashtbl.find (fst corpus.counts.triples) trigram
                  |> Option.value_map ~default:Bignum.zero ~f:Fn.id)
        done;
        let wscore =
          Hashtbl.find (fst corpus.counts.words) word |> Option.value ~default:Bignum.zero
        in
        !tscore, wscore, !triples)
    in
    let (ttotal, wtotal), e10k =
      List.fold_map e10k ~init:(Bignum.zero, Bignum.zero) ~f:(fun (t, w) word ->
        let tscore, wscore, triples = tscore_wscore_and_triples word in
        let acc = Bignum.( + ) t tscore, Bignum.( + ) w wscore in
        acc, (word, tscore, wscore, triples))
    in
    let e10k =
      List.map e10k ~f:(fun (word, tscore, wscore, triples) ->
        let tscore' = Bignum.( / ) tscore ttotal in
        let wscore' = Bignum.( / ) wscore wtotal in
        let score = combine_scores tscore' wscore' in
        word, score, tscore, wscore, triples, tscore', wscore')
      |> List.sort ~compare:(fun (_, s1, _, _, _, _, _) (_, s2, _, _, _, _, _) ->
        Bignum.compare s2 s1)
    in
    List.iter e10k ~f:(fun (word, score, _tscore, _wscore, _triples, tscore', wscore') ->
      printf
        "%s\t%s\t%s\t%s\n"
        word
        (Bignum.to_string_hum score)
        (Bignum.to_string_hum tscore')
        (Bignum.to_string_hum wscore'));
    let e10k = List.map e10k ~f:(fun (a, b, c, f, e, _, _) -> a, b, c, f, e) in
    (* printf
       "%s\n"
       ([%sexp_of: (string * float * string list) list] (List.take e10k 10)
       |> Sexp.to_string); *)
    let balanced =
      let rec loop acc total seen e10k =
        let total = ref total in
        match e10k with
        | [] -> acc
        | (word, score, _tscore, _wscore, triples) :: rest ->
          let acc = (word, score) :: acc in
          let rest =
            List.map rest ~f:(fun (word', _score', tscore', wscore', triples') ->
              let new_tscore =
                triples
                |> List.filter ~f:(Fn.non (Set.mem seen))
                |> List.fold ~init:tscore' ~f:(fun tscore' triple ->
                  if String.is_substring word' ~substring:triple
                  then (
                    let freq =
                      Hashtbl.find (fst corpus.counts.triples) triple
                      |> Option.value_map ~default:Bignum.zero ~f:Fn.id
                    in
                    (* total := Bignum.( - ) !total freq; *)
                    let _ = Bignum.( - ) tscore' freq in
                    tscore')
                  else tscore')
              in
              let new_score =
                let tscore = Bignum.( / ) new_tscore !total in
                let wscore = Bignum.( / ) wscore' (snd corpus.counts.words) in
                combine_scores tscore wscore
              in
              (* let new_score = _score' in *)
              word', new_score, new_tscore, wscore', triples')
            |> List.sort ~compare:(fun (_, s1, _, _, _) (_, s2, _, _, _) ->
              Bignum.compare s2 s1)
          in
          let seen = List.fold triples ~init:seen ~f:Set.add in
          loop acc !total seen rest
      in
      loop [] (snd corpus.counts.triples) String.Set.empty e10k
    in
    let words = balanced in
    (* let words, _ =
       List.fold
       e10k
       ~init:([], String.Set.empty)
       ~f:(fun (acc, seen) ((word, score, triples) as _x) ->
       if List.exists triples ~f:(fun triple -> not (Set.mem seen triple))
       then
       ( (* printf "%s\n%!" ([%sexp_of: string * float * string list] x |> Sexp.to_string); *)
       (word, score) :: acc
       , List.fold triples ~init:seen ~f:Set.add )
       else acc, seen)
       in *)
    let words = List.take (List.rev words) 1000 in
    let score = List.map words ~f:snd |> List.sum (module Bignum) ~f:Fn.id in
    let words = List.map words ~f:fst in
    printf "%s\n%s\n" (String.concat ~sep:" " words) (Bignum.to_string_hum score)
  in
  let _main (corpus : Corpus.t) e10k () =
    let e10k =
      Array.filter_map e10k ~f:(fun word ->
        let word_len = String.length word in
        let score = ref 0. in
        if word_len < 3
        then None
        else (
          for i = 0 to word_len - 3 do
            let j = i + 1 in
            let k = j + 1 in
            let trigram = String.of_char_list [ word.[i]; word.[j]; word.[k] ] in
            score
              := !score
                 +. (Hashtbl.find (fst corpus.freqs.triples) trigram
                     |> Option.value ~default:0.)
          done;
          Some (word, !score)))
    in
    let pre_vars, e10k = List.split_n (List.of_array e10k) 200 in
    let e10k = Array.of_list e10k in
    let vars = Array.of_list pre_vars in
    let len_e10k = Array.length e10k in
    let len_vars = Array.length vars in
    let acceptance_probability old_cost new_cost temperature =
      let ( < ) = Float.( < ) in
      let res =
        if new_cost < old_cost
        then 1.
        else Float.exp ((old_cost -. new_cost) /. temperature)
      in
      if new_cost < old_cost && Random.int 1000000 = 1
      then
        printf
          "%.20f\t%.20f\t%.12f\n%!"
          new_cost
          ((old_cost -. new_cost) /. temperature)
          temperature;
      res
    in
    let simulated_annealing
      ~objective_function
      ~make_next_solution
      ~initial_solution
      ~initial_temperature
      ~cooling_rate
      ~num_iterations
      =
      let current_solution = ref initial_solution in
      let best_solution = ref initial_solution in
      let current_cost = ref (objective_function !current_solution) in
      let best_cost = ref !current_cost in
      let temperature = ref initial_temperature in
      let force_stop = ref 0 in
      (* let max_force_stop = ref !force_stop in *)
      let iteration = ref 0 in
      while !iteration < num_iterations - 1 do
        let ( > ) = Float.( > ) in
        let ( < ) = Float.( < ) in
        let new_solution = make_next_solution !current_solution in
        let new_cost = objective_function new_solution in
        if !iteration mod 1_000_000 = 0
        then printf "iteration(%d) best_cost(%f)\n%!" !iteration !best_cost;
        if acceptance_probability !current_cost new_cost !temperature > Random.float 1.
        then (
          current_solution := new_solution;
          current_cost := new_cost);
        if new_cost < !best_cost
        then (
          (* printf
             "%f\t%d/%d\t%.12f\t%d\n%!"
             new_cost
             !iteration
             num_iterations
             !temperature
             !force_stop; *)
          (* if Int.( > ) !force_stop !max_force_stop then max_force_stop := !force_stop; *)
          force_stop := 0;
          best_solution := new_solution;
          best_cost := new_cost)
        else incr force_stop;
        temperature := !temperature *. cooling_rate;
        incr iteration
      done;
      (* printf "max_force_stop = %d\n%!" !max_force_stop; *)
      !best_solution, !best_cost
    in
    let anneal vars ~initial_temperature ~cooling_rate ~num_iterations =
      let make_next_solution vars =
        let a = Random.int len_vars in
        let b = Random.int len_e10k in
        let del = vars.(a) in
        let add = e10k.(b) in
        vars.(a) <- add;
        e10k.(b) <- del;
        vars
      in
      let objective_function vars = -1. *. Array.sum (module Float) vars ~f:snd in
      let initial_solution = vars in
      let best_solution, best_cost =
        simulated_annealing
          ~objective_function
          ~make_next_solution
          ~initial_solution
          ~initial_temperature
          ~cooling_rate
          ~num_iterations
      in
      best_cost, best_solution
    in
    let cost, solution =
      anneal
        vars
        ~initial_temperature:100.0
        ~cooling_rate:0.99999999
        ~num_iterations:1_000_000_000
    in
    printf "%s\n%f\n" (Array.map solution ~f:fst |> String.concat_array ~sep:" ") cost
  in
  let param =
    let open Command.Let_syntax in
    let%map_open corpus =
      anon ("corpus" %: string) |> Command.Param.map ~f:Corpus.load_corpus
    and _lang =
      anon ("lang" %: string)
      |> Command.Param.map ~f:(fun lang ->
        lang |> In_channel.read_all |> Jsonaf.of_string |> Mtlang.t_of_jsonaf)
    in
    let words =
      fst corpus.freqs.words
      |> Hashtbl.to_alist
      |> List.sort ~compare:(fun (_, a) (_, b) -> Float.compare b a)
      |> List.map ~f:fst
      |> List.filter ~f:(fun s ->
        String.length s >= 3
        && String.for_all s ~f:(function
          | 'a' .. 'z' | 'A' .. 'Z' | '\'' | '"' | ',' | '.' | '-' | ':' | ';' | '/' | '?'
            -> true
          | _ -> false))
      |> (fun l -> List.take l num_to_take)
      |> List.to_array
    in
    main corpus words
  in
  (* let param =
     let open Command.Let_syntax in
     let%map_open corpus = anon ("corpus" %: string)
     and lang = anon ("lang" %: string) in
     fun () ->
     let corpus = Corpus.load_corpus corpus in
     let lang = In_channel.read_all lang |> Jsonaf.of_string |> Mtlang.t_of_jsonaf in
     let table = corpus.triples in
     let len_lang = Array.length lang.words in
     let build_indexes () =
     let src = Array.init len_lang ~f:(fun _ -> true) in
     let len = ref 0 in
     let indexes = ref [] in
     while !len < 200 do
     let idx = Random.int len_lang in
     if src.(idx)
     then (
     indexes := idx :: !indexes;
     src.(idx) <- false;
     incr len)
     done;
     !indexes
     in
     let play () =
     let freqs = String.Table.create () in
     let words =
     build_indexes ()
     |> List.filter_map ~f:(fun i ->
     let word = lang.words.(i) in
     let word_len = String.length word in
     if word_len < 3
     then None
     else (
     for i = 0 to word_len - 3 do
     let j = i + 1 in
     let k = j + 1 in
     let trigram = String.of_char_list [ word.[i]; word.[j]; word.[k] ] in
     Hashtbl.update freqs trigram ~f:(function
     | None -> Hashtbl.find table trigram |> Option.value ~default:0.
     | Some x ->
     (Hashtbl.find table trigram |> Option.value ~default:0.) +. x)
     done;
     Some word))
     in
     let score = Hashtbl.data freqs |> List.sum (module Float) ~f:Fn.id in
     words, score
     in
     let words, score = play () in
     let best_words = ref words in
     let best_score = ref score in
     for _ = 0 to 200000 * 60 * 5 do
     let words, score = play () in
     if Float.( > ) score !best_score
     then (
     best_score := score;
     best_words := words)
     done;
     let s = String.concat !best_words ~sep:" " in
     printf "%s\n%.8f\n" s !best_score
     in *)
  Command.basic ~summary:"Corpus word frequency based selector thingy" param
;;

let cmd =
  let group =
    [ "gen", gen
    ; "freqs", freqs
    ; "corpus", corpus
    ; "corpus-wordfreq", corpus_wordfreq
    ; "analyze", analyze
    ]
  in
  Command.group ~summary:"StronglyTyped Keyboard Layout Analysis" group
;;
