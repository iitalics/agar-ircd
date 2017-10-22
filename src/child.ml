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

    let my_nick =
      M.get_s =>
        function
        | Waiting_nick -> "*"
        | Waiting_user n -> n
        | LoggedIn n -> n

    let with_server_prefix m =
      Msg.with_prefix
        (Msg.Prefix_server (!Irc_common.server_name))
        m

    let send_with_nick f =
      my_nick >>= fun nic ->
      send_back (Msg.to_string
                   (with_server_prefix (f nic)))


    (* implementation ********************)


    (** initialize the child actor **)
    let init () = Waiting_nick

    (** process a parsed message **)
    let recv_msg m =
      let cmd = m.Msg.raw_cmd in
      (* let params = m.Msg.raw_params in *)
      if cmd = "QUIT" then
        send_back
          (Msg.to_string
             (with_server_prefix
                (Msg.simple1 "ERROR" "Bye")))
        >> M.quit
      else
        M.get_s >>= function
        | _ ->
           (match cmd with
            (*| "NICK" ->*)
            | _ -> send_with_nick (Msg.Errors._UNKNOWNCOMMAND cmd))

    (** process just a string input **)
    let recv s =
      match CharParser.parse Msg_parse.message s with
      | Ok m -> recv_msg m
      | Bad _ -> M.return ()

    (** handle the client disconnecting **)
    let discon =
      M.return ()

  end
