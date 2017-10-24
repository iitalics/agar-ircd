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


(* simple message constructors *)

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
