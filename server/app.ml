open! Core
open! Bonsai

type actions = { set_counter : Analysis.Counter.Action.t -> unit Ui_effect.t }

type t =
  { counter : int
  ; keyboard : Analysis.Keyboard.t
  ; schedule_event : unit Ui_effect.t -> unit
  ; actions : actions
  }

let component graph =
  let open Bonsai.Let_syntax in
  let keyboard, keyboard_inject, cancel = Analysis.Keyboard.state_machine graph in
  let runtime_mode, _runtime_mode_toggle =
    Analysis.Runtime.Mode.state_machine ~default_model:`Auto graph
  in
  Analysis.Runtime.Mode.start
    runtime_mode
    ~f:
      (let%map keyboard_inject = keyboard_inject in
       keyboard_inject [ Analysis.Keyboard.Action.Random_swap ])
    graph;
  let _overwrite keys =
    let%arr keyboard_inject = keyboard_inject
    and cancel = cancel in
    let%bind.Ui_effect () = cancel in
    keyboard_inject [ Analysis.Keyboard.Action.Overwrite keys ]
  in
  let counter, set_counter = Analysis.Counter.counter 1 graph in
  let%arr counter = counter
  and set_counter = set_counter
  and keyboard = keyboard in
  fun schedule_event -> { counter; keyboard; actions = { set_counter }; schedule_event }
;;
