open Batteries

type nick_name = string
type user_name = string
type host_name = string
type chan_name = string
type mask = string

type user_mode =
  [ `i (* invisible *)
  | `w (* wallops *)
  | `o (* operator *)
  | `s (* recieves server notices *)
  ]

type stats_query = char



let server_name = ref "test.irc"
let server_version = "agar-irc-0.0.1"

let server_date =
  let open Unix in
  let tm = localtime (time ()) in
  let dow = List.at ["Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"]
              tm.tm_wday
  in
  let mon = List.at ["Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                     "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"]
              tm.tm_mon
  in
  Printf.sprintf "%s %s %d %d at %02d:%02d:%02d EDT"
    dow
    mon
    tm.tm_mday
    (tm.tm_year + 1900)
    tm.tm_hour
    tm.tm_min
    tm.tm_sec
