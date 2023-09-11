open! Import

module T = struct
  type t =
    { finger : Finger.t
    ; hand : Hand.t
    ; x : float
    ; y : float
    ; col : int
    ; row : int
    ; layer : int
    ; layer_trigger : int option
    ; modifier : bool
    }
  [@@deriving sexp, compare, hash, equal]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

module T2 = struct
  module T = struct
    type nonrec t = t * t [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module T3 = struct
  module T = struct
    type nonrec t = t * t * t [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

let hand_finger (t : t) = t.hand, t.finger
