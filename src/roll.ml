open! Import
open! Incr

type t =
  { inward : float
  ; outward : float
  }

let zero = { inward = 0.; outward = 0. }
let ( + ) a b = { inward = a.inward +. b.inward; outward = a.outward +. b.outward }
let empty = { inward = 0.; outward = 0. }

let is_same_finger (k1 : Key.t) (k2 : Key.t) =
  [%compare.equal: Finger.t] k1.finger k2.finger
;;

let direction (k1 : Key.t) (k2 : Key.t) =
  let f1 = Finger.to_int k1.finger in
  let f2 = Finger.to_int k2.finger in
  match k1.hand with
  | `L -> if f2 - f1 > 0 then `O else `I
  | `R -> if f2 - f2 > 0 then `I else `O
;;

let make keys ~bigrams =
  let tbl = Hf.Table.create () in
  List.iter keys ~f:(fun (k1 : Key.t) ->
      let v =
        List.fold keys ~init:empty ~f:(fun { inward; outward } k2 ->
            match is_same_finger k1 k2 with
            | true -> { inward; outward }
            | false ->
              let freq =
                let c1, c2 = k1.Key.code, k2.Key.code in
                match c1, c2 with
                | `Char c1, `Char c2 ->
                  String.Table.find_or_add
                    bigrams
                    (String.of_char_list [ c1; c2 ])
                    ~default:(fun () -> 0.)
              in
              (match direction k1 k2 with
              | `I -> { inward = inward +. freq; outward }
              | `O -> { inward; outward = outward +. freq }))
      in
      let hf = k1.hand, k1.finger in
      Hf.Table.update tbl hf ~f:(function
          | None -> v
          | Some { inward; outward } ->
            { inward = inward +. v.inward; outward = outward +. v.outward }));
  tbl
;;

let incr : t Hf.Table.t Incr.t =
  let open Incr.Let_syntax in
  let%bind bigrams = Config.bigrams in
  let%map foo =
    By_hr.table
    |> Hr.Table.data
    |> List.map ~f:(fun incr -> map incr ~f:(fun keys -> make keys ~bigrams))
    |> all
  in
  let tbl = Hf.Table.create () in
  List.iter foo ~f:(fun tbl' ->
      Hf.Table.merge_into ~src:tbl' ~dst:tbl ~f:(fun ~key:_ s d ->
          Set_to
            (match d with
            | None -> s
            | Some { inward; outward } ->
              { inward = inward +. s.inward; outward = outward +. s.outward })));
  tbl
;;
