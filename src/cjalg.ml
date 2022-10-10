open! Import

let ksize_max = 30

module Keyboard = struct
  type t =
    { layout : string
    ; shiftedLayout : string
    ; stats : Stats.t
    ; score : float
    }
  [@@deriving sexp]

  let nilKeyboard =
    { layout = String.make ksize_max '0'
    ; shiftedLayout = String.make ksize_max '0'
    ; stats = Stats.empty
    ; score = Float.infinity
    }
  ;;
end

module Default = struct
  let ksize = 30
  let trueksize = 30
  let sim_anneal_generations = 0
  let initFromFile = false
  let detailedOutput = true
  let file_read_not_happen = 0
  let maxRuns = Int.max_value
  let chanceExponentiator = 0.9
  let runsBeforeGtbRoundsInc = 4
  let algorithm_rounds = 16
  let chanceToUsePreviousLayout = 0.2
  let num_swaps_between_rounds = ksize / 15
  let numThreads = 1
  let runsBeforeChanceInc = 1
  let runsBeforeSwapsInc = 1
  let gtbRounds = 4
  let printTimeInterval = Time.Span.of_sec 60.
  let fitnessMax = Float.max_value
  let gtbNumberOfSwaps = 10
  let gtbRoundsBeforeSwapInc = 32
end

module Thread_arg = struct
  type t =
    { bestk : Keyboard.t
    ; numRounds : int
    ; chanceToUsePreviousLayout : float
    ; numberOfSwaps : int
    ; startTime : Time.t
    ; numThreads : int
          (* indicates that the subroutine should keep creating new threads until this reaches 0. *)
    ; isFinished : bool (* indicates whether the current thread is done running. *)
    }

  let empty =
    { bestk = Keyboard.nilKeyboard
    ; numRounds = 0
    ; chanceToUsePreviousLayout = 0.
    ; numberOfSwaps = 0
    ; startTime = Time.now ()
    ; numThreads =
        0
        (* indicates that the subroutine should keep creating new threads until this reaches 0. *)
    ; isFinished = false (* indicates whether the current thread is done running. *)
    }
  ;;
end

let runThreadsRec arg = arg
let printPercentages _ = ()
let printTime _ = ()

type m = { key : char }

let monographs = [| { key = '?' } |]
let monLen = Array.length monographs
let isSwappable _ = true
let locWithShifted _ _ = 0
let isLegalSwap _ _ _ = true
let swap _ _ _ = ()

let smartMutate k numberOfSwaps =
  let q = monLen / 4 in
  let swapslen = 2 * numberOfSwaps in
  let charsToSwap =
    Array.init swapslen ~f:(fun _ ->
        let rec loop i default =
          match i < 0 with
          | true -> default
          | false ->
            (match
               isSwappable monographs.(i).key && Random.int Int.max_value mod q = 0
             with
            | false -> loop (i - 1) default
            | true -> monographs.(i).key)
        in
        loop (monLen - 1) monographs.(0).key)
  in
  let i = ref 0 in
  let lockins = ref [] in
  while !i < swapslen do
    let lc1 = locWithShifted k charsToSwap.(!i) in
    let lc2 = locWithShifted k charsToSwap.(!i + 1) in
    lockins := (lc1, lc2) :: !lockins;
    if isLegalSwap k lc1 lc2 then swap k lc1 lc2
  done;
  k, Array.of_list_rev !lockins
;;

let calcFitness (k : Keyboard.t) = k

let buildShuffledIndices length =
  let indices = Array.init length ~f:(Fn.const 0) in
  for i = 0 to length - 1 do
    let j = Random.int Int.max_value mod (i - 1) in
    indices.(i) <- indices.(j);
    indices.(j) <- i
  done;
  indices
;;

exception Return of float

let improveLayout evaluationToBeat (k : Keyboard.t) lockins lockin_length =
  let indices = buildShuffledIndices (2 * Default.trueksize) in
  try
    for i = 0 to (2 * Default.trueksize) - 1 do
      for j = i + 1 to (2 * Default.trueksize) - 1 do
        match isLegalSwap k indices.(i) indices.(j) with
        | false -> ()
        | true ->
          let skipRound =
            let rec loop inx acc =
              match inx = lockin_length with
              | true -> acc
              | false ->
                if fst lockins.(inx) = indices.(i)
                   || fst lockins.(inx) = indices.(j)
                   || snd lockins.(inx) = indices.(i)
                   || snd lockins.(inx) = indices.(j)
                then true
                else loop (inx + 1) acc
            in
            loop 0 false
          in
          (match skipRound with
          | true -> ()
          | false ->
            swap k indices.(i) indices.(j);
            let k = calcFitness k in
            let evaluation = k.score in
            if Float.(evaluation < evaluationToBeat) then raise (Return evaluation))
      done
    done;
    evaluationToBeat
  with
  | Return ev -> ev
;;

let anneal (k : Keyboard.t) lockins lockin_length =
  let k = calcFitness k in
  let rec loop lastEvaluation evaluation =
    let lastImprovement =
      if Float.(evaluation < lastEvaluation) then lastEvaluation -. evaluation else 0.
    in
    let lastEvaluation = evaluation in
    let evaluationToBeat = lastEvaluation +. lastImprovement in
    let evaluation = improveLayout evaluationToBeat k lockins lockin_length in
    if Float.(evaluation < evaluationToBeat)
    then loop lastEvaluation evaluation
    else evaluation
  in
  k, loop k.score k.score
;;

let greatToBestThreadRec (arg : Thread_arg.t) =
  let innerArg = { arg with numThreads = arg.numThreads - 1; isFinished = false } in
  let numberOfSwaps = Default.gtbNumberOfSwaps in
  let loop_cond i =
    if arg.numThreads <= 1 then i < arg.numRounds else not innerArg.isFinished
  in
  let rec loop ~i ~(arg : Thread_arg.t) =
    match loop_cond i with
    | true ->
      let numberOfSwaps =
        if i mod Default.gtbRoundsBeforeSwapInc = Default.gtbRoundsBeforeSwapInc - 1
        then numberOfSwaps + 1
        else numberOfSwaps
      in
      let k = arg.bestk in
      (* Any swaps made by smartMutate() are "locked in" and may not be undone by anneal() *)
      let k, lockins = smartMutate k numberOfSwaps in
      (* Use lockins only half the time *)
      let k, _evaluation =
        if i mod 2 = 0 then anneal k lockins numberOfSwaps else anneal k [||] 0
      in
      let (k : Keyboard.t) = calcFitness k in
      let arg =
        if Float.(k.score < arg.bestk.score) then { arg with bestk = k } else arg
      in
      loop ~i:(i + 1) ~arg
    | false -> arg
  in
  let arg = loop ~i:0 ~arg in
  let arg =
    if Float.(innerArg.bestk.score < arg.bestk.score)
    then { arg with bestk = innerArg.bestk }
    else arg
  in
  { arg with isFinished = true }
;;

let greatToBest (k : Keyboard.t) numRounds =
  let arg =
    { Thread_arg.empty with
      bestk = k
    ; numRounds
    ; numThreads = Default.numThreads
    ; isFinished = false
    }
  in
  let arg = greatToBestThreadRec arg in
  arg.bestk
;;

let rec loop
    ~(arg : Thread_arg.t)
    ~runNum
    ~runsBeforeChanceInc
    ~runsBeforeSwapsInc
    ~gtbRounds
    ~prevBestFitness
    ~timeOnPrint
    ~printTimeInterval
  =
  match runNum = Default.maxRuns with
  | true -> ()
  | false ->
    let arg, runsBeforeChanceInc =
      match runNum mod runsBeforeChanceInc = 0 with
      | false -> arg, runsBeforeChanceInc
      | true ->
        let arg =
          { arg with
            chanceToUsePreviousLayout =
              arg.chanceToUsePreviousLayout ** Default.chanceExponentiator
          }
        in
        let runsBeforeChanceInc =
          Int.of_float (Float.of_int runsBeforeChanceInc *. 1.2) + 1
        in
        if Default.detailedOutput
        then
          printf
            "Chance to use previous layout is now %f.\n"
            arg.chanceToUsePreviousLayout;
        arg, runsBeforeChanceInc
    in
    let arg, runsBeforeSwapsInc =
      match runNum mod runsBeforeSwapsInc = 0 with
      | false -> arg, runsBeforeSwapsInc
      | true ->
        let arg = { arg with numberOfSwaps = arg.numberOfSwaps + 1 } in
        let runsBeforeSwapsInc =
          Int.of_float (Float.of_int runsBeforeSwapsInc *. 1.2) + 1
        in
        if Default.detailedOutput
        then printf "Number of swaps between rounds is now %d.\n" arg.numberOfSwaps;
        arg, runsBeforeSwapsInc
    in
    let gtbRounds =
      match runNum mod Default.runsBeforeGtbRoundsInc = 0 with
      | false -> gtbRounds
      | true ->
        let gtbRounds = gtbRounds * 2 in
        if Default.detailedOutput
        then printf "Number of rounds in greatToBest() is now %d.\n" gtbRounds;
        gtbRounds
    in
    let (arg : Thread_arg.t) = runThreadsRec arg in
    let prevBestFitness, timeOnPrint, printTimeInterval =
      if Float.(arg.bestk.score < prevBestFitness)
      then (
        let prevBestFitness = arg.bestk.score in
        printPercentages arg.bestk;
        printTime arg.startTime;
        (* If a keyboard was just printed, don't print the time for a while. *)
        let timeOnPrint =
          Time.now () |> Time.to_span_since_epoch |> Time.Span.( + ) printTimeInterval
        in
        prevBestFitness, timeOnPrint, printTimeInterval)
      else if Time.Span.( >= ) (Time.now () |> Time.to_span_since_epoch) timeOnPrint
              && Default.detailedOutput
      then (
        printTime arg.startTime;
        let timeOnPrint =
          Time.Span.( + ) (Time.now () |> Time.to_span_since_epoch) printTimeInterval
        in
        let printTimeInterval =
          (Time.Span.to_sec printTimeInterval *. 1.5) +. 1. |> Time.Span.of_sec
        in
        prevBestFitness, timeOnPrint, printTimeInterval)
      else prevBestFitness, timeOnPrint, printTimeInterval
    in
    let bestBeforeGTB = arg.bestk.score in
    let bestk = greatToBest arg.bestk gtbRounds in
    let arg = Thread_arg.{ arg with bestk } in
    let prevBestFitness =
      if Float.( < ) arg.bestk.score bestBeforeGTB
      then (
        let prevBestFitness = arg.bestk.score in
        if Default.detailedOutput then printf "\n***Found from greatToBest()***\n";
        printPercentages arg.bestk;
        printTime arg.startTime;
        prevBestFitness)
      else prevBestFitness
    in
    loop
      ~arg
      ~runNum:(runNum + 1)
      ~runsBeforeChanceInc
      ~runsBeforeSwapsInc
      ~gtbRounds
      ~prevBestFitness
      ~timeOnPrint
      ~printTimeInterval
;;

let runAlgorithm () =
  let arg = { Thread_arg.empty with bestk = Keyboard.nilKeyboard } in
  let arg =
    { arg with
      numRounds = Default.algorithm_rounds
    ; startTime = Time.now ()
    ; numThreads = Default.numThreads
    ; chanceToUsePreviousLayout = Default.chanceToUsePreviousLayout
    ; numberOfSwaps = Default.num_swaps_between_rounds
    }
  in
  let runsBeforeChanceInc = Default.runsBeforeChanceInc in
  let runsBeforeSwapsInc = Default.runsBeforeSwapsInc in
  let gtbRounds = Default.gtbRounds in
  let printTimeInterval = Default.printTimeInterval in
  let timeOnPrint =
    Time.Span.( + ) (arg.startTime |> Time.to_span_since_epoch) printTimeInterval
  in
  let prevBestFitness = Default.fitnessMax in
  loop
    ~arg
    ~runNum:0
    ~runsBeforeChanceInc
    ~runsBeforeSwapsInc
    ~gtbRounds
    ~prevBestFitness
    ~timeOnPrint
    ~printTimeInterval
;;
