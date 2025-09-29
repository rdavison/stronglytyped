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

module Keyboard = struct
  let t =
    Rpc.Rpc.create
      ~name:"get_keyboard"
      ~version:0
      ~bin_query:[%bin_type_class: Unit.t]
      ~bin_response:[%bin_type_class: Analysis.Keyboard.t]
      ~include_in_error_count:Only_on_exn
  ;;
end

module Gen = struct
  let t =
    Rpc.Rpc.create
      ~name:"gen"
      ~version:0
      ~bin_query:[%bin_type_class: Unit.t]
      ~bin_response:
        [%bin_type_class: Analysis.Keyboard.t * (float * Analysis.Keyboard.t) list]
      ~include_in_error_count:Only_on_exn
  ;;
end

module Config = struct
  let set =
    Rpc.Rpc.create
      ~name:"set_config"
      ~version:0
      ~bin_query:[%bin_type_class: Analysis.Config.t]
      ~bin_response:[%bin_type_class: Unit.t]
      ~include_in_error_count:Only_on_exn
  ;;
end
