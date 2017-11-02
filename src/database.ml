open Batteries

type con = int


type guest_entry = {
    gent_con : con;
    gent_nick : Irc.nick_name option;
    gent_user : Irc.user_name option;
    gent_real : Irc.real_name option;
  }

type guests = unit
let create_guests ~size = ()


type user_details = {
    udet_username : Irc.user_name;
    udet_realname : Irc.real_name;
    udet_addr : Unix.sockaddr;
    udet_oper : [`Dom | `Sub | `Van];
  }

type host
  = Host_there of Irc.nick_name
  | Host_here of user_details

type user_entry = {
    uent_con : con;
    uent_nick : string option;
  }

type users = unit
let create_users ~size = ()
