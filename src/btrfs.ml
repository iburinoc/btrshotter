open! Core
open Async

let prog = "btrfs"

let ensure_is_subvolume cmd_runner ~path =
  Cmd_runner.run cmd_runner ~prog ~args:[ "subvolume"; "show"; path ]
  >>| Or_error.ignore_m
;;

let create_snapshot cmd_runner ~source ~dest =
  Cmd_runner.run
    cmd_runner
    ~prog
    ~args:[ "subvolume"; "snapshot"; "create"; "-r"; source; dest ]
  >>| Or_error.ignore_m
;;

let list_entries cmd_runner ~path =
  let open Deferred.Or_error.Let_syntax in
  let%bind stats =
    Cmd_runner.run cmd_runner ~prog ~args:[ "subvolume"; "list"; "-os"; path ]
  in
  List.map stats ~f:Entry.of_subvolume_list_line
  |> Or_error.combine_errors
  |> Deferred.return
;;
