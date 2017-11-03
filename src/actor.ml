open Batteries
module DB = Database
module RPL = Replies

(**
    Actor-capable monad extension
 *)
module type MONAD = sig
  include Monad.SIG

  val get_parent_con : DB.con option t
  val get_con : DB.con t
  val send_msg : DB.con -> Irc.msg -> unit t
  (* val should_close : bool -> unit t *)

  val with_users : (DB.Users.t -> 'a) -> 'a t
  val with_guests : (DB.Guests.t -> 'a) -> 'a t

end

(**
    Signature for actor implementations
 *)
module type IMPL =
  functor(M : MONAD) -> sig

    val on_init : unit -> unit M.t
    val on_quit : unit -> unit M.t
    val on_recieve : Irc.msg -> unit M.t

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

  module Let_syntax = struct
    let bind m ~f = M.bind m f
  end

  (* polling user info **********)
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


  (* sending messages ***********)
  let send_msg_back msg =
    let%bind c = get_con in
    send_msg c msg

  let send_reply c rpl =
    get_nick_aster >>= (send_msg c % rpl)

  let send_reply_back rpl =
    get_nick_aster >>= (send_msg_back % rpl)


  (* implementation: setup/teardown ************)
  let on_init () = pure_
  let on_quit () = pure_

  (* implementation: recieve message ************)
  let on_recieve msg =
    send_reply_back
      (RPL._UNKNOWNCOMMAND msg.Irc.command)

end
