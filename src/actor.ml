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
  let on_init () = pure_
  let on_quit () = pure_

  (* implementation: recieve message ************)

  let on_recieve msg =
    let handler _ _ =
      bad (RPL._UNKNOWNCOMMAND msg.command)
    in
    match%bind handler msg.prefix msg.params with
    | Ok () -> pure_
    | Bad rpl ->
       get_nick_aster
       => rpl
       >>= with_server_prefix
       >>= send_msg_back

end
