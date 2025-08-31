open! Core
open! Bonsai_web
module Keycode = Stronglytyped_analysis.Keycode
module Key = Stronglytyped_analysis.Key

val state_machine
  :  Key.Id.t
  -> (Keycode.t * (Keycode.t -> unit Bonsai.Effect.t)) Computation.t
