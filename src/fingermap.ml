open! Import

type t =
  | Standard
  | Angle
  | Enigmak
  | Custom of Finger.t array
[@@deriving sexp]

let to_string = function
  | Standard -> "standard"
  | Angle -> "angle"
  | Enigmak -> "enigmak"
  | Custom custom ->
    Array.map custom ~f:(fun x -> x |> Finger.to_int |> Int.to_string)
    |> String.concat_array
;;

let of_string s =
  let s = s |> String.strip |> String.lowercase in
  match s with
  | "standard" -> Standard
  | "angle" -> Angle
  | "enigmak" -> Enigmak
  | _ ->
    Custom
      (s
       |> String.to_array
       |> Array.map ~f:(fun x -> x |> Char.to_string |> Int.of_string |> Finger.of_int))
;;

module Command = struct
  let default = Standard

  let mapping =
    [ (Some "standard", fun _ -> Standard)
    ; (Some "angle", fun _ -> Angle)
    ; (Some "enigmak", fun _ -> Enigmak)
    ; ( None
      , fun s ->
          let arr =
            s
            |> String.to_array
            |> Array.map ~f:(fun x ->
              x |> Char.to_string |> Int.of_string |> Finger.of_int)
          in
          Custom arr )
    ]
  ;;

  let possible_values =
    List.map mapping ~f:(fun (name, _f) ->
      match name with
      | Some name -> name
      | None ->
        "$CUSTOM where $CUSTOM is a string of numbers representing a custom fingermap")
  ;;

  let arg_type =
    Command.Arg_type.map Command.Param.string ~f:(fun s ->
      let s = s |> String.strip |> String.lowercase in
      let rec loop = function
        | [] -> assert false
        | (None, f) :: _rest -> f s
        | (Some s', f) :: rest -> if String.equal s s' then f s else loop rest
      in
      loop mapping)
  ;;
end
