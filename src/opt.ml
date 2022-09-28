open! Import
open! Incr

let anneal =
  let open Let_syntax in
  (* let kmax = config.kmax in *)
  (* let neighbour = config.neighbour in *)
  let best = ref [] in
  (* let neighbour = Neighbour.make neighbour in *)
  let actually_set_best ({ Analysis.score; pretty } as analysis) =
    printf "Score: %f\n%s\n" score pretty;
    best := analysis :: !best
  in
  let set_best (s_new : Analysis.t) =
    match !best with
    | [] -> actually_set_best s_new
    | (s_prev : Analysis.t) :: _ ->
      if Float.( < ) s_new.score s_prev.score then actually_set_best s_new else ()
  in
  let%map analysis = Analysis.incr in
  set_best analysis;
  !best
;;
