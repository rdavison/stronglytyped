let index r c = (r * 10) + (c mod 10)
let rc t r c = t.((r * 10) + (c mod 10))

let time_it f =
  let start = Time.now () in
  let res = f () in
  let finish = Time.now () in
  let span = Time.abs_diff start finish in
  span, res
;;

module Random = struct
  include Random

  let rec int2 x =
    let i, j = Random.int x, Random.int x in
    if i <> j then i, j else int2 x
  ;;
end

let layouts =
  [ "qwerty", "qwertyuiopasdfghjkl;zxcvbnm,./"
  ; "mtvap", "ypoujkdlcwinea,mhtsrqz'.;bfvgx"
  ; "alphabet", "abcdefghijklmnopqrstuvwxyz'.,;"
  ]
;;
