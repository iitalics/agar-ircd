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


module ERR = struct

  let _UNKNOWNCOMMAND c nic = Msg.simple "421" [nic; c; "Unknown command"]
  let _NEEDMOREPARAMS c nic = Msg.simple "461" [nic; c; "Not enough parameters"]
  let _ALREADYREGISTERED nic = Msg.simple "462" [nic; "You may not register"]
  let _ERRONEOUSNICKNAME bad_nic nic = Msg.simple "432" [nic; bad_nic; "Erroneous nickname"]

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
        (Msg.Prefix_server(!Irc_common.server_name))
        m

    let or_default_prefix = function
      | Some pfx -> M.return pfx
      | None ->
         my_nick =>
           fun nic ->
           (* TODO: look up user/host *)
           Msg.Prefix_user(nic, None, None)


    (* results within monads *)

    let ( >>=? ) mr f =
      mr >>= function
      | Ok x -> f x
      | Bad e -> M.return (Bad e)

    let ok_ = M.return (Ok ())
    let ok x = M.return (Ok x)
    let bad y = M.return (Bad y)

    let params_1 cmd = function
      | p::_ -> ok p
      | [] -> bad (ERR._NEEDMOREPARAMS cmd)



    (* implementation ********************)

    (** initialize the child actor **)
    let init () = Waiting_nick

    (** process a parsed message **)
    let process st prefix params = function
      | "QUIT" ->
         (* TODO: disconnect if logged in *)
         send_back
           (Msg.simple1 "ERROR" "Bye"
            |> with_server_prefix
            |> Msg.to_string)
         >> M.quit

      | "NICK" ->
         params_1 "NICK" params >>=? fun new_nick ->
         (match st with
          | Waiting_nick ->
             M.put_s (Waiting_user new_nick)
             >> ok_
          | _ ->
             bad (ERR._ERRONEOUSNICKNAME new_nick))

      | cmd ->
         bad (ERR._UNKNOWNCOMMAND cmd)

    (** extract relevant information out of message **)
    let recv_msg msg =
      M.get_s >>= fun st ->
      or_default_prefix msg.Msg.raw_pfx >>= fun prefix ->
      let cmd = msg.Msg.raw_cmd in
      let params = msg.Msg.raw_params in
      process st prefix params cmd

    (** process just a string input **)
    let recv s =
      match CharParser.parse Msg_parse.message s with
      | Bad _ -> M.return ()
      | Ok m ->
         recv_msg m >>= function
         | Ok () -> M.return ()
         | Bad msg_of_nick ->
            my_nick >>= fun nic ->
            send_back (msg_of_nick nic
                       |> with_server_prefix
                       |> Msg.to_string)

    (** handle the client disconnecting **)
    let discon =
      M.return ()

  end
