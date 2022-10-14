open! Import
open! Stronglytyped_analyzer

type t =
  { n : int
  ; bestk : Analysis.t option
  ; prev_best_fitness : float
  ; start_time : Time.t
  ; print_time_interval : Time.Span.t
  ; time_on_print : Time.Span.t
  }