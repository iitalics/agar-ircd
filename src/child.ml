open Batteries
open Irc_common

type state
  = Waiting_nick_user
  | Waiting_nick of user_name * string
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

  type t = nick_name -> Msg.t

  let _UNKNOWNCOMMAND c nic = Msg.simple "421" [nic; c; "Unknown command"]
  let _NEEDMOREPARAMS c nic = Msg.simple "461" [nic; c; "Not enough parameters"]
  let _ALREADYREGISTERED nic = Msg.simple "462" [nic; "You may not reregister"]
  let _ERRONEOUSNICKNAME bad_nic nic = Msg.simple "432" [nic; bad_nic; "Erroneous nickname"]
  let _NICKCOLLISION nic = Msg.simple "432" [nic; nic; "Nickname collision"]

end


module Make : FUNC =
  functor(M : MONAD) -> struct
    module MonadEx = Monad.Extras(M)
    module Infix = Monad.Infix(M)
    open Infix


    (* results within monads *)

    type 'a msg_try = ('a, ERR.t) Result.t M.t

    let ( >>=? ) : 'a msg_try -> ('a -> 'b msg_try) -> 'b msg_try =
      fun m f ->
      m >>= function
      | Ok x -> f x
      | Bad e -> M.return (Bad e)

    let ok_ : unit msg_try        = M.return (Ok ())
    let ok : 'a -> 'a msg_try     = fun x -> M.return (Ok x)
    let bad : ERR.t -> 'a msg_try = fun y ->  M.return (Bad y)


    (* utilities ***********************************************)

    let send_msg c msg =
      M.send c (Msg.to_string msg)

    let send_msg_back msg =
      M.con_id >>= fun c ->
      M.send c (Msg.to_string msg)

    let my_nick_opt =
      M.get_s =>
        function
        | Waiting_user n -> Some n
        | LoggedIn n -> Some n
        | _ -> None

    let or_default_prefix = function
      | Some pfx -> M.return pfx
      | None ->
         (* TODO: look up user/host *)
         my_nick_opt =>
           function
           | None      -> Msg.Prefix_user("*", None, None)
           | Some nick -> Msg.Prefix_user(nick, None, None)

    let with_server_prefix =
      Msg.with_prefix
        (Msg.Prefix_server !server_name)

    (** returns Ok(nick) if nick is available,
        or bad(f) if unavailable **)
    let nick_avail nick =
      if Msg_parse.nickname_is_valid nick then
        ok nick
      else
        bad (ERR._ERRONEOUSNICKNAME nick)

    let params_1 cmd = function
      | p::_ -> ok p
      | [] -> bad (ERR._NEEDMOREPARAMS cmd)



    (* implementation ******************************************)

    (** initialize the child actor **)
    let init () = Waiting_nick_user

    (** process just a string input **)
    let rec recv s =
      match CharParser.parse Msg_parse.message s with
      | Bad _ -> MonadEx.nop
      | Ok m ->
         recv_msg m >>= function
         | Ok () -> MonadEx.nop
         | Bad msg_of_nick ->
            my_nick_opt >>= fun o_nick ->
            let nick = Option.default "*" o_nick in
            let msg = with_server_prefix (msg_of_nick nick) in
            send_msg_back msg

    (** extract relevant information out of message **)
    and recv_msg msg =
      M.get_s >>= fun st ->
      or_default_prefix msg.Msg.raw_pfx >>= fun prefix ->
      let cmd = msg.Msg.raw_cmd in
      let params = msg.Msg.raw_params in
      process st prefix params cmd

    (** process a parsed message **)
    and process st prefix params = function

      | "QUIT" ->
         (* TODO: disconnect if logged in *)
         send_msg_back (with_server_prefix (Msg.simple1 "ERROR" "Bye"))
         >> M.quit


      | "NICK" ->
         params_1 "NICK" params >>=? fun arg ->
         nick_avail arg >>=? fun nick ->
         (match st with
          (* set nick name & wait for user name *)
          | Waiting_nick_user ->
             M.put_s (Waiting_user nick) >> ok_
          (* done w/ log-in sequence *)
          | Waiting_nick (user, real) ->
             log_in nick user real
          (* TODO: change nick while connected *)
          | _ ->
             bad ERR._NICKCOLLISION)


      | "USER" ->
         (match params with
          | u::_::_::r::_ ->
             ok (u, r)
          | _ ->
             bad (ERR._NEEDMOREPARAMS "USER"))
         >>=? fun (user, real) ->
         (match st with
          (* set user name & wait for nick name *)
          | Waiting_nick_user | Waiting_nick _ ->
             M.put_s (Waiting_nick (user, real)) >> ok_
          (* done w/ log-in sequence *)
          | Waiting_user nick ->
             log_in nick user real
          (* already registered *)
          | _ ->
             bad ERR._ALREADYREGISTERED)


      | cmd ->
         bad (ERR._UNKNOWNCOMMAND cmd)


    (** log in the user with the given nick & user name **)
    and log_in nick user real =
      M.put_s (LoggedIn nick)
      >> (M.con_id >>= send_motd)
      >> ok_


    (** send the MOTD **)
    and send_motd targ =
      let fmt = Printf.sprintf in
      List.enum [
        "375", fmt "- %s Message of the day -" !server_name;
        "372", "- Henlo and welcome to muh OCaml IRC server.";
      ]
      /@ (fun (cmd, str) -> with_server_prefix (Msg.simple1 cmd str))
      |> MonadEx.iter (fun m -> M.send targ (Msg.to_string m))

    (** handle the client disconnecting **)
    let discon =
      MonadEx.nop

  end
