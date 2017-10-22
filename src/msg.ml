open Batteries
open Irc_common

type prefix
  = Prefix_server of host_name
  | Prefix_user of nick_name * user_name option * host_name option

type t = {
    raw_pfx : prefix option;
    raw_cmd : string;
    raw_params : string list;
  }

exception Err of t


type full =
  (* user operations *)
  | PASS of string
  | NICK of nick_name
  | USER of user_name * user_mode list * string
  | OPER of string * string
  | MODE of user_mode list * user_mode list
  | SERVICE of unit
  | QUIT of string option
  | SQUIT of string * string
  | AWAY of string option

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
  | MOTD of host_name option
  | LUSERS of host_name option
  | VERSION of host_name option
  | STATS of host_name option
  | LINKS of host_name option * mask
  | TIME of host_name option
  | CONNECT of string * int * string option
  | TRACE of host_name option
  | ADMIN of host_name option
  | INFO of host_name option
  | SQUERY of string * string
  | DIE       (* optional *)
  | RESTART   (* optional *)

  (* user queries *)
  | WHO of mask * bool
  | WHOIS of host_name option * mask list
  | WHOWAS of nick_name list * int option * host_name option
  | KILL of nick_name * string

  (* misc *)
  | PING of host_name * host_name option
  | PONG of host_name * host_name option
  | ERROR of string


(** convert a message back into a string **)
let to_string m =
  let open Printf in
  let prefix_str =
    match m.raw_pfx with
    | None -> ""
    | Some (Prefix_server s) ->
       sprintf ":%s " s
    | Some (Prefix_user (nick, usr, hom)) ->
       sprintf ":%s%s%s "
         nick
         (Option.map_default ((^) "!") "" usr)
         (Option.map_default ((^) "@") "" hom)
  in
  let params_str =
    let rec f = function
      | [] -> ""
      | [x] -> " :" ^ x
      | x::xs -> " " ^ x ^ f xs
    in
    f m.raw_params
  in
  prefix_str ^ m.raw_cmd ^ params_str ^ "\r\n"

(** gives the message 'm' the prefix 'pfx' **)
let with_prefix pfx m =
  { m with raw_pfx = Some pfx }


let simple cmd pars = {
    raw_pfx = None;
    raw_cmd = cmd;
    raw_params = pars }

let simple0 cmd = {
    raw_pfx = None;
    raw_cmd = cmd;
    raw_params = [] }

let simple1 cmd par = {
    raw_pfx = None;
    raw_cmd = cmd;
    raw_params = [par] }


(** error replies **)
module Errors = struct
  open Printf

  let _UNKNOWNCOMMAND c nic = simple "421" [nic; c; "Unknown command"]
  let _NEEDMOREPARAMS c nic = simple "461" [nic; c; "Not enough parameters"]
  let _ALREADYREGISTERED nic = simple "462" [nic; "You may not register"]

end
