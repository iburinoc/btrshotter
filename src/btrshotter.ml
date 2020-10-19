open! Core
open Async

let take_snapshot
    ?(time_source = Time_source.wall_clock ())
    ~dry_run
    ~config
    ~source
    ~dest
    ()
  =
  let open Deferred.Or_error.Let_syntax in
  let cmd_runner = Cmd_runner.create ~dry_run in
  let%bind () = Btrfs.ensure_is_subvolume cmd_runner ~path:source in
  let%bind () = Btrfs.ensure_is_subvolume cmd_runner ~path:dest in
  let name = Config.create_name config (Time_source.now time_source) in
  Btrfs.create_snapshot cmd_runner ~source ~dest:(dest ^/ name)
;;

let take_snapshot_command =
  Command.async_or_error
    ~summary:"Take btrfs snapshot"
    (let open Command.Let_syntax in
    let%map_open source =
      flag "src" (required string) ~doc:" path to the subvolume to snapshot"
    and dest =
      flag
        "dst"
        (required string)
        ~doc:" path to the subvolume where the snapshots will be placed"
    and config_file = flag "config" (optional string) ~doc:" path to config file"
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
      let%bind config =
        match config_file with
        | Some file -> Reader.load_sexp file Config.t_of_sexp
        | None -> return Config.default
      in
      print_s [%message "Using config " (config : Config.t)];
      take_snapshot ~dry_run ~config ~source ~dest ())
;;

let prune_snapshots_command =
  Command.async_or_error
    ~summary:"Prune no longer needed snapshots"
    (let open Command.Let_syntax in
    let%map_open path =
      flag "path" (required string) ~doc:" path to the subvolume where the snapshots are"
    and config_file = flag "config" (optional string) ~doc:" path to config file"
    and force = flag "force" no_arg ~doc:" whether to actually remove the snapshots" in
    fun () ->
      let open Deferred.Or_error.Let_syntax in
      print_s [%message "Pruning subvolumes from " (path : string)];
      let%bind () =
        if (not (Int.equal (Unix.getuid ()) 0)) && force
        then Deferred.return (error_s [%message "Must be run as root"])
        else return ()
      in
      let%bind config =
        match config_file with
        | Some file -> Reader.load_sexp file Config.t_of_sexp
        | None -> return Config.default
      in
      print_s [%message "Using config " (config : Config.t)];
      let cmd_runner = Cmd_runner.create ~dry_run:false in
      let%bind entries = Btrfs.list_entries cmd_runner ~path in
      let `Keep _, `Remove to_remove =
        Config.prune_entries config entries ~now:(Time_ns.now ())
      in
      let to_remove_names = List.map to_remove ~f:Entry.name in
      if not force
      then (
        print_s [%message "Would remove: " ~_:(to_remove_names : string list)];
        return ())
      else (
        print_s [%message "Removing: " ~_:(to_remove_names : string list)];
        Deferred.Or_error.List.iter to_remove_names ~how:`Sequential ~f:(fun name ->
            Btrfs.remove_snapshot cmd_runner ~path:(path ^/ name))))
;;

let show_retentions_command =
  Command.async_or_error
    ~summary:"Show reasons each snapshot is kept"
    (let open Command.Let_syntax in
    let%map_open path =
      flag "path" (required string) ~doc:" path to the subvolume where the snapshots are"
    and config_file = flag "config" (optional string) ~doc:" path to config file" in
    fun () ->
      let open Deferred.Or_error.Let_syntax in
      print_s [%message "Getting retentions from " (path : string)];
      let%bind () =
        if not (Int.equal (Unix.getuid ()) 0)
        then Deferred.return (error_s [%message "Must be run as root"])
        else return ()
      in
      let%bind config =
        match config_file with
        | Some file -> Reader.load_sexp file Config.t_of_sexp
        | None -> return Config.default
      in
      print_s [%message "Using config " (config : Config.t)];
      let cmd_runner = Cmd_runner.create ~dry_run:false in
      let%bind entries = Btrfs.list_entries cmd_runner ~path in
      let retentions = Config.get_retentions config entries ~now:(Time_ns.now ()) in
      print_s [%sexp (retentions : Entry.t list Config.Time_span.Map.t)];
      return ())
;;

let dump_default_config_command =
  Command.async
    ~summary:"Dump default config"
    (let open Command.Let_syntax in
    return (fun () ->
        print_s [%sexp (Config.default : Config.t)];
        Deferred.return ()))
;;

let command =
  Command.group
    ~summary:"Manage btrfs snapshots"
    [ "take", take_snapshot_command
    ; "prune", prune_snapshots_command
    ; "show", show_retentions_command
    ; "dump-default-config", dump_default_config_command
    ]
;;

let%expect_test "dry run looks good" =
  let%bind () =
    take_snapshot
      ~time_source:(Time_source.create ~now:Time_ns.epoch () |> Time_source.read_only)
      ~dry_run:true
      ~config:Config.default
      ~source:"/path/to/source"
      ~dest:"/path/to/dest"
      ()
    >>| ok_exn
  in
  [%expect
    {|
    btrfs subvolume show /path/to/source
    btrfs subvolume show /path/to/dest
    btrfs subvolume snapshot -r /path/to/source /path/to/dest/1970-01-01_00.00 |}]
;;
