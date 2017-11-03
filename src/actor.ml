open Batteries
module DB = Database
module RPL = Replies
open Irc

(**
    Actor-capable monad extension
 *)
module type MONAD = sig
  include Monad.SIG

  (** returns the connection id of this actor's connection. *)
  val get_con : DB.con t

  (** returns the connection id of the parent node to this
      connection, if any. *)
  val get_parent_con : DB.con option t

  (** returns this server's nick name, used to identify themself
      with other servers. *)
  val get_server_name : nick_name t

  (** [with_users f] / [with_guests f] applies the pure function [f]
      to the users / guests database, returning the result. *)
  val with_users : (DB.Users.t -> 'a) -> 'a t
  val with_guests : (DB.Guests.t -> 'a) -> 'a t

  (** [mut_users f] / [mut_guests f] mutates the database by applying
      the pure function [f] to the users / guests database, changing
      it to the result. *)
  val mut_users : (DB.Users.t -> DB.Users.t) -> unit t
  val mut_guests : (DB.Guests.t -> DB.Guests.t) -> unit t

  (** [send_msg c msg] sends message [m] to connection [c]. *)
  val send_msg : DB.con -> msg -> unit t

  (* val should_close : bool -> unit t *)

end

(**
    Signature for actor implementations
 *)
module type IMPL =
  functor(M : MONAD) -> sig

    val on_init : unit -> unit M.t
    val on_quit : unit -> unit M.t
    val on_recieve : msg -> unit M.t

  end


(**
   Implementation of actor
 *)
module Impl(M : MONAD) = struct
  module Ex = Monad.Extra(M)
  module Res = Monad.Result(M)
  open M
  open Ex
  open Res

  let ok_ : (unit, RPL.reply) result M.t = pure (Ok ())

  module Let_syntax = struct
    let bind m ~f = M.bind m f
    let map m ~f = M.map f m
  end


  (* polling user/server info **********)

  let get_guest_entry =
    let%bind c = get_con in
    with_guests (DB.Guests.by_con c)
  let get_user_entry =
    let%bind c = get_con in
    with_users (DB.Users.by_con c)

  let mod_guest_entry f =
    let%bind c = get_con in
    mut_guests (DB.Guests.modify c f)
  let mod_user_entry f =
    let%bind c = get_con in
    mut_users (DB.Users.modify c f)

  let is_guest = get_guest_entry => Option.is_some
  let is_user =  get_user_entry => Option.is_some

  let get_nick_opt =
    let%bind c = get_con in
    match%bind (with_guests @@ DB.Guests.by_con c) with
    | Some ge ->
       pure (ge.DB.gent_nick)
    | None ->
       (with_users @@ DB.Users.by_con c) =>
         Option.map (fun ue -> ue.DB.uent_nick)

  let get_nick_aster =
    get_nick_opt => Option.default "*"

  let get_server_prefix =
    let%map snic = get_server_name in
    Prefix.of_nick
      (Printf.sprintf "%s.%s"
         snic
         !server_domain)

  let with_server_prefix msg =
    get_server_prefix => fun pfx -> { msg with prefix = pfx }


  (* sending messages ***********)
  let send_msg_back msg =
    let%bind c = get_con in
    send_msg c msg

  let send_reply c rpl =
    get_nick_aster >>= (send_msg c % rpl)

  let send_reply_back rpl =
    let%bind c = get_con in
    send_reply c rpl


  (* implementation: setup/teardown ************)
  let on_init () =
    let%bind c = get_con in
    mut_guests (DB.Guests.insert
                  { DB.gent_con = c;
                    DB.gent_nick = None;
                    DB.gent_user = None;
                    DB.gent_real = None })

  let on_quit () =
    let%bind c = get_con in
    mut_guests (DB.Guests.modify c @@ const None)
    >> mut_users (DB.Users.modify c @@ const None)


  (**
     client command: CAP
   *)
  let cmd_CAP = function
    | "LS"::_ -> send_msg_back (Msg.simple "CAP" ["*"; "LS"; ""]) >> ok_
    | "LIST"::_ -> send_msg_back (Msg.simple "CAP" ["*"; "LIST"; ""]) >> ok_
    | c::_ -> bad (RPL._INVALIDCAPCMD c)
    | []   -> bad (RPL._INVALIDCAPCMD "?")


  (**
     client command: USER
   *)
  let cmd_USER = function
    | usr::_::_::real::_ ->
       if%bind is_guest then
         mod_guest_entry
           (fun ge -> Some { ge
                        with DB.gent_user = Some usr;
                             DB.gent_real = Some real })
         >> ok_
       else
         bad RPL._ALREADYREGISTERED

    | _ -> bad (RPL._NEEDMOREPARAMS "USER")



  (* implementation: recieve message ************)

  let on_recieve msg =
    (* TOOD: are we always gonna throw the prefix away? *)
    let handler = match msg.command with
      | "CAP" -> cmd_CAP
      | "USER" -> cmd_USER
      | cmd -> fun _ -> bad (RPL._UNKNOWNCOMMAND cmd)
    in
    match%bind handler msg.params with
    | Ok () -> pure_
    | Bad rpl ->
       get_nick_aster
       => rpl
       >>= with_server_prefix
       >>= send_msg_back

end
