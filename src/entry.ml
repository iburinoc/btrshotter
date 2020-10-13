open! Core
open! Async

module T = struct
  type t =
    { creation_time : Time_ns.t
    ; name : string
    }
  [@@deriving fields, sexp_of, compare]
end

include T
include Comparable.Make_plain (T)

let create = Fields.create

let of_subvolume_list_line line =
  let open Or_error.Let_syntax in
  let items = String.split line ~on:' ' in
  let%bind () =
    if Int.(List.length items >= 14)
    then Ok ()
    else error_s [%message "Invalid snapshot stats line " (line : string)]
  in
  let%bind create_date =
    Or_error.try_with (fun () -> List.nth_exn items 10 |> Date.of_string)
  in
  let%bind create_time =
    Or_error.try_with (fun () -> List.nth_exn items 11 |> Time_ns.Ofday.of_string)
  in
  let zone = Lazy.force Timezone.local in
  let creation_time = Time_ns.of_date_ofday ~zone create_date create_time in
  let path = String.concat ~sep:" " (List.drop items 13) in
  let name = String.split ~on:'/' path |> List.last_exn in
  Ok { creation_time; name }
;;
