let index r c = (r * 10) + (c mod 10)
let rc t r c = t.((r * 10) + (c mod 10))

let time_it f =
  let start = Time.now () in
  let res = f () in
  let finish = Time.now () in
  let span = Time.abs_diff start finish in
  span, res
;;

let rec rand2 ~cache x =
  let i, j = Random.int x, Random.int x in
  let return y =
    cache := Some y;
    y
  in
  if i <> j
  then (
    match !cache with
    | None -> return (i, j)
    | Some (i', j') -> if i = i' && j = j' then rand2 ~cache x else return (i, j))
  else rand2 ~cache x
;;