open! Core
open! Async_rpc_kernel

module Version : sig
  val t : (unit, string) Rpc.Rpc.t
end

module Keyboard : sig
  val t : (unit, Analysis.Keyboard.t) Rpc.Rpc.t
end

module Gen : sig
  val t : (unit, Analysis.Keyboard.t * (float * Analysis.Keyboard.t) list) Rpc.Rpc.t
end
