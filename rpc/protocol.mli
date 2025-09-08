open! Core
open! Async_rpc_kernel

module Version : sig
  val t : (unit, string) Rpc.Rpc.t
end
