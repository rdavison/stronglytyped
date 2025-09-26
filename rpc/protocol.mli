open! Core
open! Async_rpc_kernel

module Version : sig
  val t : (unit, string) Rpc.Rpc.t
end

module Keyboard : sig
  val t : (unit, Analysis.Keyboard.t) Rpc.Rpc.t
end
