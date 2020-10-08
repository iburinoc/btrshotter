open! Core
open Async

val ensure_is_subvolume :
  Cmd_runner.t -> path:string -> unit Or_error.t Deferred.t
