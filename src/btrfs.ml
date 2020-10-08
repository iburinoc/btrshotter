open! Core
open Async

let ensure_is_subvolume cmd_runner ~path =
  Cmd_runner.run cmd_runner ~prog:"btrfs" ~args:[ "subvolume"; "show"; path ]
  >>| Or_error.ignore_m
;;
