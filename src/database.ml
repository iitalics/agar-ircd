open Batteries
open Irc_common

type user_info = {
    user_name : string;
    host_name : string;
    real_name : string;
    nick_name : string;
    mutable modes : user_mode list;
  }

let is_oper u = List.mem `o u.modes
let is_invis u = List.mem `i u.modes


module type SIG = sig

  type user_db

  val create_user_db : capacity_hint:int -> user_db
  val user_exists : nick:string -> user_db -> bool
  val user_route : nick:string -> user_db -> int option
  val user_info : nick:string -> user_db -> user_info option
  val add_user : nick:string -> int -> user_info option -> user_db -> unit
  val del_user : nick:string -> user_db -> unit

end


module Hash_DB = struct

  type entry = int * user_info option
  type user_db = (string, entry) Hashtbl.t

  let create_user_db ~capacity_hint:cap =
    Hashtbl.create cap

  let user_exists ~nick db =
    Hashtbl.mem db nick

  let user_route ~nick db =
    Option.map fst (Hashtbl.find_option db nick)

  let user_info ~nick db =
    Option.bind (Hashtbl.find_option db nick) snd

  let add_user ~nick route nfo db =
    Hashtbl.add db nick (route, nfo)

  let del_user ~nick db =
    Hashtbl.remove_all db nick

end
