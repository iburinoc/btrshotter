open! Core
open Async

let run ~source ~dest ~dry_run =
  let open Deferred.Or_error.Let_syntax in
  let cmd_runner = Cmd_runner.create ~dry_run in
  let%bind () = Btrfs.ensure_is_subvolume cmd_runner ~path:source in
  let%bind () = Btrfs.ensure_is_subvolume cmd_runner ~path:dest in
  return ()
;;

let command =
  Command.async_or_error ~summary:"Manage btrfs snapshots"
    (let open Command.Let_syntax in
    let%map_open source =
      flag "source" (required string) ~doc:" path to the subvolume to snapshot"
    and dest =
      flag "dest" (required string)
        ~doc:" path to the subvolume where the snapshots will be placed"
    and dry_run =
      flag "dry-run" no_arg
        ~doc:
          " whether to run in dry-run mode (no commands will actually be run \
           in dry-run mode)"
    in
    fun () ->
      print_s
        [%message
          "Snapshotting subvolume from source to dest "
            (source : string)
            (dest : string)];
      run ~source ~dest ~dry_run)
;;

let%expect_test "dry run looks good" =
  run ~source:"/path/to/source" ~dest:"/path/to/dest" ~dry_run:true >>| ok_exn;
  [%expect {|
    btrfs subvolume show /path/to/source
    btrfs subvolume show /path/to/dest |}]
;;
