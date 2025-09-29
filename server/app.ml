open! Core
open! Bonsai
open! Bonsai.Let_syntax

type t =
  { keyboard : Analysis.Keyboard.t
  ; window : (float * Analysis.Keyboard.t) list
  ; schedule_event : unit Ui_effect.t -> unit
  ; set_corpus : Analysis.Corpus.t -> unit
  }

let component graph =
  let corpus_var = Bonsai.Expert.Var.create Analysis.Corpus.fast in
  let corpus = Bonsai.Expert.Var.value corpus_var in
  let keyboard, window = Analysis.Gen_actor.gen ~corpus graph in
  let%arr window = window
  and keyboard = keyboard in
  fun schedule_event ->
    { window; keyboard; schedule_event; set_corpus = Bonsai.Expert.Var.set corpus_var }
;;
