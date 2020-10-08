open Core
open Async

module Stdout = struct
  type t = string list
end

type t = { dry_run : bool }

let create ~dry_run = { dry_run }

let run { dry_run } ~prog ~args =
  let open Deferred.Or_error.Let_syntax in
  print_endline (String.concat ~sep:" " (prog :: args));
  if dry_run then return [] else Process.run_lines ~prog ~args ()
;;
