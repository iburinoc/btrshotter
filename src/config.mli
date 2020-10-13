open! Core
open! Async

type t [@@deriving sexp]

val default : t
val create_name : t -> Time_ns.t -> string

val prune_entries
  :  t
  -> Entry.t list
  -> now:Time_ns.t
  -> [ `Keep of Entry.t list ] * [ `Remove of Entry.t list ]
