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
    ; stats = Stats.worst
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
  let runsBeforeChanceInc = 1
  let runsBeforeSwapsInc = 1
  let gtbRounds = 4
  let printTimeInterval = Time_float.Span.of_sec 60.
  let fitnessMax = Float.max_value
  let gtbNumberOfSwaps = 10
  let gtbRoundsBeforeSwapInc = 32
end

let printPercentages _ = ()
let printTime _ = ()

type m = { key : char }

let monographs = [| { key = '?' } |]
let monLen = Array.length monographs
let isSwappable _ = true
let locWithShifted _ _ = 0
let isLegalSwap _ _ _ = true
let swap k _ _ = k
let calcFitness (k : Keyboard.t) = k

let smartMutate (k : Keyboard.t) numberOfSwaps =
  let q = monLen / 4 in
  let swapslen = 2 * numberOfSwaps in
  let charsToSwap =
    Array.init swapslen ~f:(fun _ ->
      let rec loop i default =
        match i < 0 with
        | true -> default
        | false ->
          (match isSwappable monographs.(i).key && Random.int Int.max_value mod q = 0 with
           | false -> loop (i - 1) default
           | true -> monographs.(i).key)
      in
      loop (monLen - 1) monographs.(0).key)
  in
  let k, lockins =
    let rec loop i k lockins =
      if i < swapslen
      then k, lockins
      else (
        let lc1 = locWithShifted k charsToSwap.(i) in
        let lc2 = locWithShifted k charsToSwap.(i + 1) in
        let lockins = (lc1, lc2) :: lockins in
        let k = if isLegalSwap k lc1 lc2 then swap k lc1 lc2 else k in
        loop (i + 1) k lockins)
    in
    loop 0 k []
  in
  k, Array.of_list_rev lockins
;;

let buildShuffledIndices length =
  let indices = Array.init length ~f:(Fn.const 0) in
  for i = 0 to length - 1 do
    let j = Random.int Int.max_value mod (i - 1) in
    indices.(i) <- indices.(j);
    indices.(j) <- i
  done;
  indices
;;

let improveLayout evaluationToBeat (k : Keyboard.t) lockins lockin_length =
  let indices = buildShuffledIndices (2 * Default.trueksize) in
  let rec loop_i i =
    let rec loop_j j =
      match j < 2 * Default.trueksize with
      | false -> `Continue
      | true ->
        (match isLegalSwap k indices.(i) indices.(j) with
         | false -> loop_j (j + 1)
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
            | true -> loop_j (j + 1)
            | false ->
              let k = calcFitness (swap k indices.(i) indices.(j)) in
              let evaluation = k.score in
              if Float.(evaluation < evaluationToBeat)
              then `Stop evaluation
              else loop_j (j + 1)))
    in
    match i < 2 * Default.trueksize with
    | false -> evaluationToBeat
    | true ->
      (match loop_j (i + 1) with
       | `Stop x -> x
       | `Continue -> loop_i (i + 1))
  in
  loop_i 0
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

let newKeeb () = Keyboard.nilKeyboard

let smartMutateAndAnneal
  ~chanceToUsePreviousLayout
  ~numberOfSwaps
  ~startTime
  ~numRounds
  ~bestk
  =
  let rec loop i (prevk, bestk) =
    match i < numRounds with
    | false -> bestk
    | true ->
      let prevk' =
        if i > 0 && Float.(Random.float 1. < chanceToUsePreviousLayout)
        then fst (smartMutate prevk numberOfSwaps)
        else newKeeb ()
      in
      let k, score = anneal prevk [||] 0 in
      let bestk' =
        if Float.(score < bestk.Keyboard.score)
        then (
          printPercentages k;
          printTime startTime;
          k)
        else bestk
      in
      loop (i + 1) (prevk', bestk')
  in
  let bestk' = loop 0 (Keyboard.nilKeyboard, bestk) in
  if Float.(bestk'.Keyboard.score < bestk.score) then bestk' else bestk
;;

let updateNumberOfSwaps i ~numberOfSwaps =
  if i mod Default.gtbRoundsBeforeSwapInc = Default.gtbRoundsBeforeSwapInc - 1
  then numberOfSwaps + 1
  else numberOfSwaps
;;

let greatToBest (bestk : Keyboard.t) ~numRounds =
  let rec loop ~i ~(bestk : Keyboard.t) ~numberOfSwaps =
    match i < numRounds with
    | false -> bestk
    | true ->
      let numberOfSwaps = updateNumberOfSwaps i ~numberOfSwaps in
      (* Any swaps made by smartMutate() are "locked in" and may not be undone by anneal() *)
      let mutated, lockins = smartMutate bestk numberOfSwaps in
      let k =
        let k, _ =
          (* Use lockins only half the time *)
          if i mod 2 = 0
          then anneal mutated lockins numberOfSwaps
          else anneal mutated [||] 0
        in
        calcFitness k
      in
      let bestk = if Float.(k.score < bestk.score) then k else bestk in
      loop ~i:(i + 1) ~bestk ~numberOfSwaps
  in
  let bestk' = loop ~i:0 ~bestk ~numberOfSwaps:Default.gtbNumberOfSwaps in
  if Float.(bestk'.score < bestk.score) then bestk' else bestk
;;

let updatePrevChances i ~runsBeforeChanceInc ~chanceToUsePreviousLayout =
  match i mod runsBeforeChanceInc = 0 with
  | false -> chanceToUsePreviousLayout, runsBeforeChanceInc
  | true ->
    let chanceToUsePreviousLayout =
      chanceToUsePreviousLayout ** Default.chanceExponentiator
    in
    let runsBeforeChanceInc =
      Int.of_float (Float.of_int runsBeforeChanceInc *. 1.2) + 1
    in
    if Default.detailedOutput
    then printf "Chance to use previous layout is now %f.\n" chanceToUsePreviousLayout;
    chanceToUsePreviousLayout, runsBeforeChanceInc
;;

let updateSwapsChances i ~runsBeforeSwapsInc ~numberOfSwaps =
  match i mod runsBeforeSwapsInc = 0 with
  | false -> numberOfSwaps, runsBeforeSwapsInc
  | true ->
    let numberOfSwaps = numberOfSwaps + 1 in
    let runsBeforeSwapsInc = Int.of_float (Float.of_int runsBeforeSwapsInc *. 1.2) + 1 in
    if Default.detailedOutput
    then printf "Number of swaps between rounds is now %d.\n" numberOfSwaps;
    numberOfSwaps, runsBeforeSwapsInc
;;

let updateGtbRounds i ~gtbRounds =
  match i mod Default.runsBeforeGtbRoundsInc = 0 with
  | false -> gtbRounds
  | true ->
    let gtbRounds = gtbRounds * 2 in
    if Default.detailedOutput
    then printf "Number of rounds in greatToBest() is now %d.\n" gtbRounds;
    gtbRounds
;;

let updateMiscellaneous ~bestk ~prevBestFitness ~startTime ~printTimeInterval ~timeOnPrint
  =
  let is_better = Float.(bestk.Keyboard.score < prevBestFitness) in
  let ts = Time_float.now () |> Time_float.to_span_since_epoch in
  let timeOnPrint' =
    let ( + ) = Time_float.Span.( + ) in
    ts + printTimeInterval
  in
  if is_better
  then (
    let prevBestFitness = bestk.score in
    printPercentages bestk;
    printTime startTime;
    (* If a keyboard was just printed, don't print the time for a while. *)
    prevBestFitness, timeOnPrint', printTimeInterval)
  else if Time_float.Span.(ts >= timeOnPrint) && Default.detailedOutput
  then (
    printTime startTime;
    let printTimeInterval =
      (Time_float.Span.to_sec printTimeInterval *. 1.5) +. 1. |> Time_float.Span.of_sec
    in
    prevBestFitness, timeOnPrint', printTimeInterval)
  else prevBestFitness, timeOnPrint, printTimeInterval
;;

let runAlgorithm () =
  let startTime = Time_float.now () in
  let numRounds = Default.algorithm_rounds in
  let rec loop
    i
    ~runsBeforeChanceInc
    ~runsBeforeSwapsInc
    ~gtbRounds
    ~prevBestFitness
    ~timeOnPrint
    ~printTimeInterval
    ~chanceToUsePreviousLayout
    ~numberOfSwaps
    ~bestk
    =
    match i < Default.maxRuns with
    | false -> ()
    | true ->
      let chanceToUsePreviousLayout, runsBeforeChanceInc =
        updatePrevChances i ~runsBeforeChanceInc ~chanceToUsePreviousLayout
      in
      let numberOfSwaps, runsBeforeSwapsInc =
        updateSwapsChances i ~runsBeforeSwapsInc ~numberOfSwaps
      in
      let gtbRounds = updateGtbRounds i ~gtbRounds in
      let bestk =
        smartMutateAndAnneal
          ~chanceToUsePreviousLayout
          ~numberOfSwaps
          ~startTime
          ~numRounds
          ~bestk
      in
      let prevBestFitness, timeOnPrint, printTimeInterval =
        updateMiscellaneous
          ~bestk
          ~prevBestFitness
          ~startTime
          ~printTimeInterval
          ~timeOnPrint
      in
      let bestBeforeGTB = bestk.score in
      let bestk = greatToBest bestk ~numRounds:gtbRounds in
      let prevBestFitness =
        if Float.(bestk.score < bestBeforeGTB)
        then (
          let prevBestFitness = bestk.score in
          if Default.detailedOutput then printf "\n***Found from greatToBest()***\n";
          printPercentages bestk;
          printTime startTime;
          prevBestFitness)
        else prevBestFitness
      in
      loop
        (i + 1)
        ~runsBeforeChanceInc
        ~runsBeforeSwapsInc
        ~gtbRounds
        ~prevBestFitness
        ~timeOnPrint
        ~printTimeInterval
        ~chanceToUsePreviousLayout
        ~numberOfSwaps
        ~bestk
  in
  let printTimeInterval = Default.printTimeInterval in
  let timeOnPrint =
    Time_float.Span.( + ) (startTime |> Time_float.to_span_since_epoch) printTimeInterval
  in
  loop
    0
    ~runsBeforeChanceInc:Default.runsBeforeChanceInc
    ~runsBeforeSwapsInc:Default.runsBeforeSwapsInc
    ~gtbRounds:Default.gtbRounds
    ~prevBestFitness:Default.fitnessMax
    ~timeOnPrint
    ~printTimeInterval
    ~chanceToUsePreviousLayout:Default.chanceToUsePreviousLayout
    ~numberOfSwaps:Default.num_swaps_between_rounds
    ~bestk:Keyboard.nilKeyboard
;;
