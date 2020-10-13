open! Core
open! Async

module type Retention_span = sig
  module Id : sig
    type t [@@deriving sexp_of, compare]
  end

  val to_id : Time_ns.t -> Id.t
  val ids_to_keep : now:Time_ns.t -> keep:int -> Id.t list
end

module type Retainer = sig
  val entries_to_keep : Entry.t list -> now:Time_ns.t -> keep:int -> Entry.t list
end

module Make_retainer (T : Retention_span) : Retainer = struct
  module Id = struct
    include T.Id
    include Comparable.Make_plain (T.Id)
  end

  let entries_to_keep entries ~now ~keep =
    let ids_to_keep = T.ids_to_keep ~now ~keep in
    let entries_by_id =
      List.map entries ~f:(fun entry -> T.to_id (Entry.creation_time entry), entry)
      |> Id.Map.of_alist_multi
      |> Map.map ~f:(fun entries ->
             List.sort entries ~compare:Entry.compare |> List.hd_exn)
    in
    List.filter_map ids_to_keep ~f:(Map.find entries_by_id)
  ;;
end

module Time_span = struct
  module T = struct
    type t =
      | Hourly
      | Daily
      | Weekly
      | Monthly
      | Yearly
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  module Hourly_retention_span = struct
    module Id = struct
      type t = Date.t * int [@@deriving sexp_of, compare]
    end

    let to_id time =
      let date, ofday = Time_ns.to_date_ofday time ~zone:Timezone.utc in
      let hour = (Time_ns.Ofday.to_parts ofday).hr in
      date, hour
    ;;

    let ids_to_keep ~now ~keep =
      List.init keep ~f:(fun hours -> Time_ns.Span.of_int_sec (-hours * 3600))
      |> List.map ~f:(Time_ns.add now)
      |> List.map ~f:to_id
    ;;
  end

  let to_date = Time_ns.to_date ~zone:Timezone.utc

  module Daily_retention_span = struct
    module Id = struct
      type t = Date.t [@@deriving sexp_of, compare]
    end

    let to_id = to_date

    let ids_to_keep ~now ~keep =
      let today = to_date now in
      List.init keep ~f:(fun days -> Date.add_days today (-days))
    ;;
  end

  module Weekly_retention_span = struct
    module Id = struct
      type t =
        { year : int
        ; week : int
        }
      [@@deriving sexp_of, compare]
    end

    let date_to_id date =
      let week, year = Date.week_number_and_year date in
      { Id.year; week }
    ;;

    let to_id time = to_date time |> date_to_id

    let ids_to_keep ~now ~keep =
      let today = to_date now in
      List.init keep ~f:(fun days -> Date.add_days today (-days * 7))
      |> List.map ~f:date_to_id
    ;;
  end

  module Monthly_retention_span = struct
    module Id = struct
      type t =
        { year : int
        ; month : Month.t
        }
      [@@deriving sexp_of, compare]
    end

    let date_to_id date =
      let year = Date.year date in
      let month = Date.month date in
      { Id.year; month }
    ;;

    let to_id time = to_date time |> date_to_id

    let ids_to_keep ~now ~keep =
      let today = to_date now in
      List.init keep ~f:(fun months -> Date.add_months today (-months))
      |> List.map ~f:date_to_id
    ;;
  end

  module Yearly_retention_span = struct
    module Id = struct
      type t = int [@@deriving sexp_of, compare]
    end

    let date_to_id = Date.year
    let to_id time = to_date time |> date_to_id

    let ids_to_keep ~now ~keep =
      let year = to_id now in
      List.init keep ~f:(fun years -> year - years)
    ;;
  end

  let retainers =
    let (retainers : (t * (module Retainer)) list) =
      [ Hourly, (module Make_retainer (Hourly_retention_span))
      ; Daily, (module Make_retainer (Daily_retention_span))
      ; Weekly, (module Make_retainer (Weekly_retention_span))
      ; Monthly, (module Make_retainer (Monthly_retention_span))
      ; Yearly, (module Make_retainer (Yearly_retention_span))
      ]
    in
    Map.of_alist_exn retainers
  ;;
end

module Retention_spec = struct
  type t = int Time_span.Map.t [@@deriving sexp]
end

type t =
  { naming_scheme : string
  ; retention : Retention_spec.t
  }
[@@deriving sexp]

let default =
  { naming_scheme = "%F_%H.%M"
  ; retention =
      [ Time_span.Hourly, 24; Daily, 14; Monthly, 12; Yearly, 100 ]
      |> Time_span.Map.of_alist_exn
  }
;;

let create_name t time =
  let time =
    Time_ns.to_span_since_epoch time
    |> Time_ns.Span.to_sec
    |> if am_running_test then Unix.gmtime else Unix.localtime
  in
  Core.Unix.strftime time t.naming_scheme
;;

let prune_entries t entries ~now =
  let keep =
    Map.to_alist t.retention
    |> List.concat_map ~f:(fun (span, keep) ->
           let (module Retainer) = Map.find_exn Time_span.retainers span in
           Retainer.entries_to_keep entries ~now ~keep)
    |> Entry.Set.of_list
  in
  let remove = List.filter entries ~f:(fun entry -> not (Set.mem keep entry)) in
  `Keep (Set.to_list keep), `Remove remove
;;

let%expect_test "default config looks good" =
  print_s [%sexp (default : t)];
  [%expect
    {|
      ((naming_scheme %F_%H.%M)
       (retention ((Hourly 24) (Daily 14) (Monthly 12) (Yearly 100)))) |}]
;;

let%expect_test "pruning looks good" =
  let to_time = Time_ns.of_string_gen ~if_no_timezone:(`Use_this_one Timezone.utc) in
  let to_entry s = Entry.create ~creation_time:(to_time s) ~name:s in
  let entries =
    [ "2020-10-12 23:05:00"
    ; "2020-10-12 23:03:00"
    ; "2020-10-12 23:01:00"
    ; "2020-10-12 22:05:00"
    ; "2020-10-12 21:05:00"
    ; "2020-10-12 20:05:00"
    ; "2020-10-12 00:05:00"
    ; "2020-10-11 00:05:00"
    ; "2020-10-10 00:05:00"
    ; "2020-10-09 00:05:00"
    ; "2020-10-01 00:05:00"
    ; "2020-09-17 00:05:00"
    ; "2020-09-01 00:05:00"
    ; "2020-08-30 00:05:00"
    ; "2020-01-01 00:05:00"
    ; "2019-01-01 00:05:00"
    ]
    |> List.map ~f:to_entry
  in
  let retention =
    [ Time_span.Hourly, 3; Daily, 2; Monthly, 2; Yearly, 1 ] |> Time_span.Map.of_alist_exn
  in
  let `Keep keep, `Remove remove =
    prune_entries
      { naming_scheme = ""; retention }
      entries
      ~now:(to_time "2020-10-12 23:05:00")
  in
  let keep = List.map keep ~f:Entry.name in
  let remove = List.map remove ~f:Entry.name in
  print_s [%message "" (keep : string list) (remove : string list)];
  [%expect
    {|
    ((keep
      ("2020-01-01 00:05:00" "2020-09-01 00:05:00" "2020-10-01 00:05:00"
       "2020-10-11 00:05:00" "2020-10-12 00:05:00" "2020-10-12 21:05:00"
       "2020-10-12 22:05:00" "2020-10-12 23:01:00"))
     (remove
      ("2020-10-12 23:05:00" "2020-10-12 23:03:00" "2020-10-12 20:05:00"
       "2020-10-10 00:05:00" "2020-10-09 00:05:00" "2020-09-17 00:05:00"
       "2020-08-30 00:05:00" "2019-01-01 00:05:00"))) |}]
;;
