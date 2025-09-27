open! Core
open! Bonsai
open! Bonsai.Let_syntax

type t =
  { keyboard : Analysis.Keyboard.t
  ; window : (float * Analysis.Keyboard.t) list
  ; schedule_event : unit Ui_effect.t -> unit
  }

let component graph =
  let keyboard, window = Analysis.Gen_actor.gen graph in
  let%arr window = window
  and keyboard = keyboard in
  fun schedule_event -> { window; keyboard; schedule_event }
;;
