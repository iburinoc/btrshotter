open! Core
open Async

(* FIXME: Use config to generate name *)
let create_name time =
  let time =
    Time_ns.to_span_since_epoch time
    |> Time_ns.Span.to_sec
    |> if am_running_inline_test then Unix.gmtime else Unix.localtime
  in
  Core.Unix.strftime time "%F_%H.%M"
;;

let take_snapshot ?(time_source = Time_source.wall_clock ()) ~dry_run ~source ~dest () =
  let open Deferred.Or_error.Let_syntax in
  let cmd_runner = Cmd_runner.create ~dry_run in
  let%bind () = Btrfs.ensure_is_subvolume cmd_runner ~path:source in
  let%bind () = Btrfs.ensure_is_subvolume cmd_runner ~path:dest in
  let name = create_name (Time_source.now time_source) in
  Btrfs.create_snapshot cmd_runner ~source ~dest:(dest ^/ name)
;;

let command =
  Command.async_or_error
    ~summary:"Manage btrfs snapshots"
    (let open Command.Let_syntax in
    let%map_open source =
      flag "src" (required string) ~doc:" path to the subvolume to snapshot"
    and dest =
      flag
        "dst"
        (required string)
        ~doc:" path to the subvolume where the snapshots will be placed"
    and dry_run =
      flag
        "dry-run"
        no_arg
        ~doc:
          " whether to run in dry-run mode (no commands will actually be run in dry-run \
           mode)"
    in
    fun () ->
      let open Deferred.Or_error.Let_syntax in
      print_s
        [%message
          "Snapshotting subvolume from source to dest " (source : string) (dest : string)];
      let%bind () =
        if (not (Int.equal (Unix.getuid ()) 0)) && not dry_run
        then Deferred.return (error_s [%message "Must be run as root"])
        else return ()
      in
      take_snapshot ~dry_run ~source ~dest ())
;;

let%expect_test "dry run looks good" =
  print_s [%sexp (am_running_test : bool)];
  let%bind () =
    take_snapshot
      ~time_source:(Time_source.create ~now:Time_ns.epoch () |> Time_source.read_only)
      ~dry_run:true
      ~source:"/path/to/source"
      ~dest:"/path/to/dest"
      ()
    >>| ok_exn
  in
  [%expect
    {|
    true
    btrfs subvolume show /path/to/source
    btrfs subvolume show /path/to/dest
    btrfs subvolume snapshot create -r /path/to/source /path/to/dest/1969-12-31_19.00 |}]
;;
