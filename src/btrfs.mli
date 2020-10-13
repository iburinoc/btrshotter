open! Core
open Async

val ensure_is_subvolume : Cmd_runner.t -> path:string -> unit Or_error.t Deferred.t

val create_snapshot
  :  Cmd_runner.t
  -> source:string
  -> dest:string
  -> unit Or_error.t Deferred.t

val list_entries : Cmd_runner.t -> path:string -> Entry.t list Or_error.t Deferred.t
val remove_snapshot : Cmd_runner.t -> path:string -> unit Or_error.t Deferred.t
