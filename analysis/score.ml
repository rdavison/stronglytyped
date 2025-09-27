open! Core
open! Bonsai
open! Bonsai.Let_syntax

let score
      ~(same_finger_stats :
         ( Stats_same_finger.Typed_variant.Packed.t
           , Stats_same_finger.t
           , Stats_same_finger.Typed_variant.Packed.comparator_witness )
           Map.t
           Bonsai.t)
  =
  let%arr same_finger_stats = same_finger_stats in
  let speed =
    Map.find same_finger_stats (Stats_same_finger.Typed_variant.Packed.pack Speed)
  in
  Option.bind speed ~f:(function
    | Sfb _ | Sfs _ | Sfb_worst _ | Sfs_worst _ | Speed_worst _ -> None
    | Speed speed -> Some (speed.total *. 100.))
;;
