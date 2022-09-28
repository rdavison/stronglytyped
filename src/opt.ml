open! Import

let acceptance_curve ?(a = 3.) ?(b = 20.) x =
  (1. -. (x ** (1. /. a))) *. (Float.cos (x *. Float.pi *. b) +. 1.) /. 2.
;;

let anneal =
  let true_best = ref [] in
  let accepted_best = ref [] in
  let set_best ({ Analysis.score; layout = _; pretty } as analysis) =
    printf "Score: %f\n%s\n" score pretty;
    accepted_best := analysis :: !accepted_best;
    (match !true_best with
    | [] -> true_best := analysis :: !true_best
    | (true_best' : Analysis.t) :: _ ->
      if Float.( < ) score true_best'.score
      then true_best := analysis :: !true_best
      else ());
    !accepted_best, !true_best
  in
  let accept_or_reject pct s_old s_new =
    match Float.( < ) s_new s_old with
    | true -> `Accept
    | false ->
      let probability = acceptance_curve pct in
      if Float.( < ) (Random.float 1.0) probability then `Accept else `Reject
  in
  Incr.map2 Config.progress Analysis.incr ~f:(fun progress s_new ->
      match !accepted_best with
      | [] -> set_best s_new
      | (s_old : Analysis.t) :: _ ->
        (match accept_or_reject progress s_old.score s_new.score with
        | `Accept -> set_best s_new
        | `Reject -> !accepted_best, !true_best))
;;
