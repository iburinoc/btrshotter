open Core
open Async

module Stdout : sig
  type t = string list
end

type t

val create : dry_run:bool -> t

val run : t -> prog:string -> args:string list -> Stdout.t Or_error.t Deferred.t
