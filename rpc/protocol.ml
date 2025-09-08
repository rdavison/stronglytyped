open! Core
open! Async_rpc_kernel

module Version = struct
  let t =
    Rpc.Rpc.create
      ~name:"get_version"
      ~version:0
      ~bin_query:[%bin_type_class: Unit.t]
      ~bin_response:[%bin_type_class: String.t]
      ~include_in_error_count:Only_on_exn
  ;;
end
