open! Core
open! Async

type t [@@deriving sexp_of]

val create : creation_time:Time_ns.t -> name:string -> t
val of_subvolume_list_line : string -> t Or_error.t
val creation_time : t -> Time_ns.t
val name : t -> string
