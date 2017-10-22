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
