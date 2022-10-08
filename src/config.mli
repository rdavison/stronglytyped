open! Import

module Var : sig
  module C : sig
    val sfb : (float * float) Finger.Map.t Incr.Var.t
    val dsfb : (float * float) Finger.Map.t Incr.Var.t
    val roll : (float * float * float) Incr.Var.t
    val lsb : (float * float) Incr.Var.t
    val uf : (float * float) Incr.Var.t
    val speed : (float * float) Finger.Map.t Incr.Var.t
    val keyfreq : float Finger.Table.t Incr.Var.t
  end

  val neighbor_v : Neighbor.t Incr.Var.t
  val kmax_v : int Incr.Var.t
  val progress : float Incr.Var.t
  val gen : bool Incr.Var.t
  val temperature : float Incr.Var.t
  val cooling_factor : float Incr.Var.t
end

module Incr : sig
  module C : sig
    val sfb : (float * float) Finger.Map.t Incr.t
    val dsfb : (float * float) Finger.Map.t Incr.t
    val keyfreq : float Finger.Table.t Incr.t
    val roll : (float * float * float) Incr.t
    val lsb : (float * float) Incr.t
    val uf : (float * float) Incr.t
    val speed : (float * float) Finger.Map.t Incr.t
  end

  val neighbor : Neighbor.t Incr.t
  val kmax : int Incr.t
  val progress : float Incr.t
  val gen : bool Incr.t
  val temperature : float Incr.t
  val cooling_factor : float Incr.t
end
