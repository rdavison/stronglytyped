open! Core

module Color = struct
  let convert_hex_to_rgb =
    let f s = Int.of_string ("0x" ^ s) in
    fun (`Hex s) ->
      let s = String.lowercase s in
      let r = String.slice s 1 3 in
      let g = String.slice s 3 5 in
      let b = String.slice s 5 7 in
      `RGB (f r, f g, f b)
  ;;
end
