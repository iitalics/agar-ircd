open Batteries

type con = int
module Con = Int

type user_details =
  { udet_username : Irc.user_name;
    udet_realname : Irc.real_name;
    udet_addr : Unix.sockaddr;
    udet_oper : [`Dom | `Sub | `Van] }

type host
  = Host_there of Irc.nick_name * con
  | Host_here of user_details

type guest_entry =
  { gent_con : con;
    gent_nick : Irc.nick_name option;
    gent_user : Irc.user_name option;
    gent_real : Irc.real_name option }

type user_entry =
  { uent_con : con;
    uent_nick : string;
    uent_host : host }


(**
   Module for database of guests: users
   who have recently connected and have not
   picked a username and a nickname.
 *)
module Guests = struct
  module M = Map.Make(Con)

  type t = guest_entry M.t

  let empty : t = M.empty

  (** [by_con c guests] returns [Some ge], where [ge] is the
      guest entry with connection id [c], or [None] if that
      connection is not a guest. *)
  let by_con c gs =
    try
      Some (M.find c gs)
    with Not_found -> None

end

(**
   Module for database of users how are
   connected somewhere on the network. Note that
   operators are users too.
 *)
module Users = struct
  module StrM = Map.Make(String)
  module ConM = Map.Make(Con)

  type t = user_entry ConM.t * con StrM.t

  let empty = ConM.empty, StrM.empty

  (** [by_con c users] returns [Some ue], where [ue] is the
      user entry with connection id [c], or [None] if that
      connection is not a user. *)
  let by_con c ((us, _) : t) =
    try
      Some (ConM.find c us)
    with Not_found -> None

  (** [by_nick nick users] returns [Some ue], where [ue] is the
      user entry with nickname [nick], or [None] if that nick
      does not exist. *)
  let by_nick nic ((us, ns) : t) =
    try
      Some (ConM.find (StrM.find nic ns) us)
    with Not_found -> None

end
