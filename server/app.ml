open! Import

type t =
  { keyboard : Stem.Keyboard.t
  ; window : (float * Stem.Keyboard.t) list
  ; schedule_event : unit Ui_effect.t -> unit
  ; set_corpus : Stem.Corpus.t -> unit
  ; clear_window : unit -> unit
  ; set_optimizer : Stem.Optimizer.t -> unit
  }

let component graph =
  let corpus_var = Bonsai.Expert.Var.create Stem.Corpus.fast in
  let corpus = Bonsai.Expert.Var.value corpus_var in
  let action_var = Bonsai.Expert.Var.create Stem.Optimizer.Random in
  let action = Bonsai.Expert.Var.value action_var in
  let%arr keyboard, window, window_reset =
    Stem.Optimizer.component ~action ~corpus graph
  in
  fun schedule_event ->
    { window
    ; keyboard
    ; schedule_event
    ; set_corpus = Bonsai.Expert.Var.set corpus_var
    ; clear_window = (fun () -> schedule_event window_reset)
    ; set_optimizer = Bonsai.Expert.Var.set action_var
    }
;;
