open! Import

module T = struct
  type t = Hand.t * Finger.t [@@deriving sexp, compare, equal, bin_io]
end

include T
include Comparable.Make_binable (T)

let all = [ `l, `p; `l, `r; `l, `m; `l, `i; `r, `i; `r, `m; `r, `r; `r, `p ]
let to_string (h, f) = sprintf "%s%s" (Hand.to_string h) (Finger.to_string f)

module Tuple2 = struct
  module T = struct
    type t = T.t * T.t [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T)
end
