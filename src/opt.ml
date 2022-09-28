open! Import

let anneal =
  let best = ref [] in
  let actually_set_best ({ Analysis.score; layout = _; pretty } as analysis) =
    printf "Score: %f\n%s\n" score pretty;
    best := analysis :: !best
  in
  let set_best (s_new : Analysis.t) =
    match !best with
    | [] -> actually_set_best s_new
    | (s_prev : Analysis.t) :: _ ->
      if Float.( < ) s_new.score s_prev.score then actually_set_best s_new else ()
  in
  let open Incr.Let_syntax in
  let%map analysis = Analysis.incr in
  set_best analysis;
  !best
;;
