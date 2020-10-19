open! Core
open! Async

module Time_span : sig
  type t =
    | Hourly
    | Daily
    | Weekly
    | Monthly
    | Yearly
  [@@deriving sexp_of, compare]

  include Comparable.S with type t := t
end

type t [@@deriving sexp]

val default : t
val create_name : t -> Time_ns.t -> string

val prune_entries
  :  t
  -> Entry.t list
  -> now:Time_ns.t
  -> [ `Keep of Entry.t list ] * [ `Remove of Entry.t list ]

val get_retentions : t -> Entry.t list -> now:Time_ns.t -> Entry.t list Time_span.Map.t
