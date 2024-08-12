open! Import
open Jsonaf.Export

type t =
  { name : string
  ; no_lazy_mode : bool [@key "noLazyMode"]
  ; ordered_by_frequency : bool [@key "orderedByFrequency"]
  ; words : string array
  }
[@@deriving jsonaf]
