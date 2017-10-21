open Batteries
open Irc_common

type raw = {
    raw_pfx : string option;
    raw_cmd : string;
    raw_params : string list;
  }

type t =
  (* user operations *)
  | PASS of string
  | NICK of nick_name
  | USER of string * user_mode list * string
  | OPER of string * string
  | MODE of user_mode list * user_mode list
  | SERVICE of unit
  | QUIT of string option
  | SQUIT of string * string
  | AWAY of string option       (* optional *)

  (* channel operations *)
  | JOIN of (chan_name * string option) list
  | PART of chan_name list * string option
  | TOPIC of chan_name * string option
  | NAMES of chan_name list
  | LIST of chan_name list
  | INVITE of nick_name * chan_name
  | KICK of chan_name list

  (* sending messages *)
  | PRIVMSG of string * string
  | NOTICE of string * string

  (* server queries & commands *)
  | MOTD of server_name option
  | LUSERS of server_name option
  | VERSION of server_name option
  | STATS of server_name option
  | LINKS of server_name option * mask
  | TIME of server_name option
  | CONNECT of string * int * string option
  | TRACE of server_name option
  | ADMIN of server_name option
  | INFO of server_name option
  | SQUERY of string * string
  | DIE       (* optional *)
  | RESTART   (* optional *)

  (* user queries *)
  | WHO of mask * bool
  | WHOIS of server_name option * mask list
  | WHOWAS of nick_name list * int option * server_name option
  | KILL of nick_name * string

  (* misc *)
  | PING of server_name * server_name option
  | PONG of server_name * server_name option
  | ERROR of string
