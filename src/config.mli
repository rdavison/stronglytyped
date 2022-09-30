open! Import

module Vars : sig
  module C : sig
    val sfb : float Incr.Var.t
    val dsfb : float Incr.Var.t
    val roll : float Incr.Var.t
    val lsb : float Incr.Var.t
    val shb : float Incr.Var.t
    val shs : float Incr.Var.t
    val speed : float Incr.Var.t
    val keyfreq : float Finger.Table.t Incr.Var.t
  end

  val neighbour_v : Neighbour.t Incr.Var.t
  val kmax_v : int Incr.Var.t
  val progress : float Incr.Var.t
end

module Incr : sig
  module C : sig
    val sfb : float Incr.t
    val dsfb : float Incr.t
    val keyfreq : float Finger.Table.t Incr.t
    val roll : float Incr.t
    val lsb : float Incr.t
    val speed : float Incr.t
    val shb : float Incr.t
    val shs : float Incr.t
  end

  val neighbour : Neighbour.t Incr.t
  val kmax : int Incr.t
  val progress : float Incr.t
end
