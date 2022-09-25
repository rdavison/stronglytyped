open! Import
open! Incr

let neighbour _k =
  let i, j = rand2 ~cache:(ref None) Root.length in
  Root.swap i j
;;

let anneal ~kmax ~config =
  let best = ref None in
  let actually_set_best ({ Analysis.score; pretty } as analysis) =
    printf "Score: %f\n%s\n" score pretty;
    best := Some analysis
  in
  let set_best (_s_prev : Analysis.t) (s_new : Analysis.t) =
    match !best with
    | None -> actually_set_best s_new
    | Some (s : Analysis.t) ->
      if Float.( < ) s_new.score s.score then actually_set_best s_new else ()
  in
  let node = Incr.observe (Analysis.v ~config) in
  let observations = ref 0 in
  Incr.Observer.on_update_exn node ~f:(fun updated ->
      (match updated with
      | Initialized _ | Invalidated -> ()
      | Changed (p, n) -> set_best p n);
      observations := !observations + 1);
  let span, _ =
    time_it (fun () ->
        for k = 1 to kmax do
          neighbour k;
          Incr.stabilize ()
        done)
  in
  !observations, span
;;
