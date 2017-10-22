open Batteries

type nick_name = string
type user_name = string
type host_name = string
type chan_name = string
type mask = string

type user_mode = [ `a | `i | `w | `r | `o | `O | `s ]
type stats_query = [ `l | `m | `o | `u ]
