open! Import

type t =
  { keyboard : Stem.Keyboard.t
  ; window : (float * Stem.Keyboard.t) list
  ; schedule_event : unit Ui_effect.t -> unit
  ; dot : string
  ; set_corpus : Stem.Corpus.t -> unit
  ; clear_window : unit -> unit
  ; set_optimizer : Stem.Optimizer.t -> unit
  ; set_dot : string -> unit
  ; tick_optimizer : unit -> unit
  }

let k = ref 0

let component graph =
  let corpus_var = Bonsai.Expert.Var.create Stem.Corpus.fast in
  let corpus = Bonsai.Expert.Var.value corpus_var in
  let action_var = Bonsai.Expert.Var.create Stem.Optimizer.Random in
  let action = Bonsai.Expert.Var.value action_var in
  let keyboard, keyboard_inject, _keyboard_clear = Stem.Keyboard.state_machine graph in
  let keyboard, window, window_reset, tick =
    Stem.Optimizer.component
      ~keyboard
      ~keyboard_inject
      ~live_keyboard_inject:(Bonsai.return (fun _ -> Bonsai.Effect.Ignore))
      ~action
      ~corpus
      graph
  in
  let dot_var = Bonsai.Expert.Var.create "" in
  let dot = Bonsai.Expert.Var.value dot_var in
  let%arr keyboard = keyboard
  and window = window
  and window_reset = window_reset
  and dot = dot
  and tick = tick in
  fun schedule_event ->
    { window
    ; keyboard
    ; schedule_event
    ; dot
    ; set_corpus = Bonsai.Expert.Var.set corpus_var
    ; clear_window = (fun () -> schedule_event window_reset)
    ; set_optimizer = Bonsai.Expert.Var.set action_var
    ; set_dot = Bonsai.Expert.Var.set dot_var
    ; tick_optimizer = (fun () -> schedule_event tick)
    }
;;
