open! Core

module Maps = struct
  type 'a t =
    { a : 'a Char.Map.t
    ; ab : 'a String.Map.t
    ; aba : 'a String.Map.t
    ; abxab : 'a String.Map.t
    ; abxba : 'a String.Map.t
    ; abc : 'a String.Map.t
    ; abcd : 'a String.Map.t
    ; axc : 'a String.Map.t
    ; vbcv : 'a String.Map.t
    }
  [@@deriving sexp, equal]
end

module Bigrams = struct
  type 'a t =
    { ab : 'a
    ; aba : 'a
    ; axc : 'a
    ; vbcv : 'a
    }
  [@@deriving sexp, equal]
end

type t =
  { count : int Maps.t
  ; freq : float Maps.t
  }
[@@deriving sexp, equal]

let empty =
  let count =
    { Maps.a = Map.empty (module Char)
    ; ab = Map.empty (module String)
    ; aba = Map.empty (module String)
    ; abxab = Map.empty (module String)
    ; abxba = Map.empty (module String)
    ; abc = Map.empty (module String)
    ; abcd = Map.empty (module String)
    ; axc = Map.empty (module String)
    ; vbcv = Map.empty (module String)
    }
  in
  let freq =
    { Maps.a = Map.empty (module Char)
    ; ab = Map.empty (module String)
    ; aba = Map.empty (module String)
    ; abxab = Map.empty (module String)
    ; abxba = Map.empty (module String)
    ; abc = Map.empty (module String)
    ; abcd = Map.empty (module String)
    ; axc = Map.empty (module String)
    ; vbcv = Map.empty (module String)
    }
  in
  { count; freq }
;;

let is_vowel (x : char) =
  match Char.lowercase x with
  | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> true
  | _ -> false
;;

let of_string (s : string) =
  let len = String.length s in
  let a_count = ref (Map.empty (module Char)) in
  let ab_count = ref (Map.empty (module String)) in
  let aba_count = ref (Map.empty (module String)) in
  let abxab_count = ref (Map.empty (module String)) in
  let abxba_count = ref (Map.empty (module String)) in
  let abc_count = ref (Map.empty (module String)) in
  let abcd_count = ref (Map.empty (module String)) in
  let vbcv_count = ref (Map.empty (module String)) in
  let axc_count = ref (Map.empty (module String)) in
  let incr map_ref key =
    map_ref
    := Map.update !map_ref key ~f:(function
         | None -> 1
         | Some x -> x + 1)
  in
  for i = 0 to len - 1 do
    let a = s.[i] in
    incr a_count a;
    try
      let b = s.[i + 1] in
      let ab = String.of_list [ a; b ] in
      incr ab_count ab;
      try
        let c = s.[i + 2] in
        let abc = String.of_list [ a; b; c ] in
        let axc = String.of_list [ a; c ] in
        incr abc_count abc;
        incr axc_count axc;
        if Char.equal a c && not (Char.equal a b) then incr aba_count ab;
        try
          let d = s.[i + 3] in
          let abcd = String.of_list [ a; b; c; d ] in
          incr abcd_count abcd;
          if is_vowel a && is_vowel d
          then (
            let vbcv = String.of_list [ b; c ] in
            incr vbcv_count vbcv)
        with
        | Invalid_argument _ -> ()
      with
      | Invalid_argument _ -> ()
    with
    | Invalid_argument _ -> ()
  done;
  let a_count = !a_count in
  let ab_count = !ab_count in
  let aba_count = !aba_count in
  let abxab_count = !abxab_count in
  let abxba_count = !abxba_count in
  let abc_count = !abc_count in
  let abcd_count = !abcd_count in
  let vbcv_count = !vbcv_count in
  let axc_count = !axc_count in
  let a_total = List.sum (module Int) (Map.data a_count) ~f:Fn.id in
  let ab_total = List.sum (module Int) (Map.data ab_count) ~f:Fn.id in
  let aba_total = List.sum (module Int) (Map.data aba_count) ~f:Fn.id in
  let abxab_total = List.sum (module Int) (Map.data abxab_count) ~f:Fn.id in
  let abxba_total = List.sum (module Int) (Map.data abxba_count) ~f:Fn.id in
  let abc_total = List.sum (module Int) (Map.data abc_count) ~f:Fn.id in
  let abcd_total = List.sum (module Int) (Map.data abcd_count) ~f:Fn.id in
  let vbcv_total = List.sum (module Int) (Map.data vbcv_count) ~f:Fn.id in
  let axc_total = List.sum (module Int) (Map.data axc_count) ~f:Fn.id in
  let a_freq = Map.map a_count ~f:(fun x -> Float.of_int x /. Float.of_int a_total) in
  let ab_freq = Map.map ab_count ~f:(fun x -> Float.of_int x /. Float.of_int ab_total) in
  let aba_freq =
    Map.map aba_count ~f:(fun x -> Float.of_int x /. Float.of_int aba_total)
  in
  let abxab_freq =
    Map.map abxab_count ~f:(fun x -> Float.of_int x /. Float.of_int abxab_total)
  in
  let abxba_freq =
    Map.map abxba_count ~f:(fun x -> Float.of_int x /. Float.of_int abxba_total)
  in
  let abc_freq =
    Map.map abc_count ~f:(fun x -> Float.of_int x /. Float.of_int abc_total)
  in
  let abcd_freq =
    Map.map abcd_count ~f:(fun x -> Float.of_int x /. Float.of_int abcd_total)
  in
  let vbcv_freq =
    Map.map vbcv_count ~f:(fun x -> Float.of_int x /. Float.of_int vbcv_total)
  in
  let axc_freq =
    Map.map axc_count ~f:(fun x -> Float.of_int x /. Float.of_int axc_total)
  in
  let count =
    { Maps.a = a_count
    ; ab = ab_count
    ; aba = aba_count
    ; abxab = abxab_count
    ; abxba = abxba_count
    ; abc = abc_count
    ; abcd = abcd_count
    ; vbcv = vbcv_count
    ; axc = axc_count
    }
  in
  let freq =
    { Maps.a = a_freq
    ; ab = ab_freq
    ; aba = aba_freq
    ; abxab = abxab_freq
    ; abxba = abxba_freq
    ; abc = abc_freq
    ; abcd = abcd_freq
    ; vbcv = vbcv_freq
    ; axc = axc_freq
    }
  in
  { count; freq }
;;

let bigrams (t : t) (bigram : string) : float Bigrams.t =
  let f where = Map.find where bigram |> Option.value ~default:0. in
  let maps = t.freq in
  { ab = f maps.ab; aba = f maps.aba; axc = f maps.axc; vbcv = f maps.vbcv }
;;

let foo =
  of_string
    {|No one in the world needs an elephant tusk but an elephant.

A semitrailer is legally defined as a vehicle designed so that a portion of its weight rests on a towing vehicle. This distinguishes it from a full trailer on which the entire load, except for a drawbar, rests on its own wheels.

Though my story's seldom told, I have squandered my resistance. For a pocketful of mumbles such are promises, all lies and jest. Still, a man hears what he wants to hear and disregards the rest.

Alone now, I leaned over the edge of my boat and looked down to the bottom of the sea. The volcano was gone. The water's calm surface reflected the blue of the sky. Little waves - like silk pajamas fluttering in a breeze - lapped against the side of the boat. There was nothing else. I stretched out in the bottom of the boat and closed my eyes, waiting for the rising tide to carry me where I belonged.

You gave me wings and made me fly, you touched my hand. I could touch the sky. I lost my faith, you gave it back to me, you said no star was out of reach.

I wasn't like every other kid, you know, who dreams about being an astronaut. I was always more interested in what bark was made out of on a tree. Richard Gere's a real hero of mine. Sting. Sting would be another person who's a hero. The music that he's created over the years, I don't really listen to it, but the fact that he's making it, I respect that. I care desperately about what I do. Do I know what product I'm selling? No. Do I know what I'm doing today? No. But I'm here, and I'm gonna give it my best shot.

And sometimes when he looks at me, I know he needs you, you're all that he sees.

The charges against the accused are as follows: That he did knowingly, and in full awareness of the illegality of his actions, produce a patronus charm in the presence of a muggle.

How can an Image-Fiction writer hope to make people more critical of televisual culture by parodying television as a self-serving commercial enterprise when Pepsi and Subaru and FedEx parodies of self-serving commercials are already doing big business?

I knew it was you. You have your father's eyes. - And my mother's ears, but the rest belongs to you.

As the United States assumed greater status as a power in world politics, Americans came to believe that the nation's actions on the world stage should be guided by American political and moral principles.
|}
;;
