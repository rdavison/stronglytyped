open! Import

module Config = struct
  type kind =
    [ `Const of int
    | `Linear
    | `Curved of int
    | `Random of int list
    ]

  type t =
    { max_swaps_at_once : int
    ; kind : kind
    }

  let make ?(max_swaps_at_once = 10) kind =
    let () =
      match kind with
      | `Const _ | `Linear | `Random _ -> ()
      | `Curved a ->
        if a > 0 && a mod 2 = 0
        then ()
        else raise_s [%message "arg must be a positive power of 2" ~arg:(a : int)]
    in
    { max_swaps_at_once; kind }
  ;;
end

type t = float -> unit

let make (t : Config.t) : t =
  let curve a x =
    Float.of_int t.max_swaps_at_once *. ((1. -. x) ** a) |> Float.round_up |> Float.to_int
  in
  let num_swaps : float -> int =
    match t.kind with
    | `Const n -> Fn.const n
    | `Linear -> curve 1.
    | `Curved a -> curve (Int.to_float a)
    | `Random lst ->
      let arr = Array.of_list lst in
      let len = Array.length arr in
      fun _ -> arr.(Random.int len)
  in
  fun x ->
    for _ = 1 to num_swaps x do
      let i, j = Random.int2 Root.length in
      Root.swap i j
    done
;;
