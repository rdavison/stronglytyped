open! Import
open! Async_rpc_kernel

module Version : sig
  val t : (unit, string) Rpc.Rpc.t
end

module Gen : sig
  val t : (unit, Keyboard.t * (float * Keyboard.t) list) Rpc.Rpc.t
end

module Keyboard : sig
  val t : (unit, Keyboard.t) Rpc.Rpc.t
end

module Config : sig
  val t : (Config.t, unit) Rpc.Rpc.t
end

module Optimizer : sig
  val t : (Optimizer.t, unit) Rpc.Rpc.t
end
