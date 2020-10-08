open! Core
open Async

let command =
  Command.async_or_error ~summary:"Manage btrfs snapshots"
    (let open Command.Let_syntax in
    return (fun () ->
        let open Deferred.Or_error.Let_syntax in
        print_s [%message "Hello"];
        return ()))
