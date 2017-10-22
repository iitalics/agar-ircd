open Batteries
open Irc_common

type state
  = Waiting_nick
  | Waiting_user of nick_name
  | LoggedIn of nick_name

module type MONAD = sig
  include Monad.SIG

  val con_id : int t
  val get_s : state t
  val put_s : state -> unit t

  val send : int -> string -> unit t
  val quit : 'a t

end


module type FUNC =
  functor(M : MONAD) -> sig

    val init : unit -> state
    val recv : string -> unit M.t
    val discon : unit M.t

  end


module Make : FUNC =
  functor(M : MONAD) -> struct
    module Infix = Monad.Infix(M)
    open Infix


    (* utilities *************************)


    let send_back s =
      M.con_id >>= fun i ->
      M.send i s


    (* implementation ********************)


    (** initialize the child actor **)
    let init () = Waiting_nick

    (** process a parsed message **)
    let recv_msg m =
      if m.Msg.raw_cmd = "QUIT" then
        M.quit
      else
        let m' = Msg.with_prefix (Msg.Prefix_server "irc.node") m in
        let str =  Msg.to_string m' ^ "\r\n" in
        send_back str

    (** process just a string input **)
    let recv s =
      match CharParser.parse Msg_parse.message s with
      | Ok m -> recv_msg m
      | Bad _ -> M.return ()

    (** handle the client disconnecting **)
    let discon =
      M.return ()

  end
