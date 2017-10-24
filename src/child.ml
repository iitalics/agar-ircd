open Batteries
open Irc_common

type state
  = Waiting of (user_name * string) option * nick_name option
  | Logged_in of nick_name

module type MONAD = sig
  include Monad.SIG

  module DB : Database.SIG

  val con_id : int t
  val on_users : (DB.user_db -> 'a) -> 'a t
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

  let _UNKNOWNCOMMAND cmd nic = Msg.simple "421" [nic; cmd; "Unknown command"]
  let _NEEDMOREPARAMS cmd nic = Msg.simple "461" [nic; cmd; "Not enough parameters"]
  let _NOTREGISTERED nic = Msg.simple "451" [nic; "You have not registered"]

  let _ALREADYREGISTERED nic = Msg.simple "462" [nic; "You may not reregister"]
  let _ERRONEOUSNICKNAME bad_nic nic = Msg.simple "432" [nic; bad_nic; "Erroneous nickname"]
  let _NICKNAMEINUSE bad_nic nic = Msg.simple "433" [nic; bad_nic; "Nickname is already in use"]
  let _NICKCOLLISION nic = Msg.simple "432" [nic; nic; "Nickname collision"]

  let _NOSUCHNICK bad_nic nic = Msg.simple "401" [nic; bad_nic; "No such nick/channel"]
  let _NOSUCHCHANNEL chan nic = Msg.simple "401" [nic; chan; "No such channel"]

end


module Make : FUNC =
  functor(M : MONAD) -> struct
    module MonadEx = Monad.Extras(M)
    module Infix = Monad.Infix(M)
    open Infix


    (* result-carrying monad *)

    type 'a msg_try = ('a, ERR.t) Result.t M.t

    let ok_ : unit msg_try        = M.return (Ok ())
    let ok : 'a -> 'a msg_try     = fun x -> M.return (Ok x)
    let bad : ERR.t -> 'a msg_try = fun y ->  M.return (Bad y)

    let ( >>=? ) : 'a msg_try -> ('a -> 'b msg_try) -> 'b msg_try =
      fun m f ->
      m >>= function
      | Ok x -> f x
      | Bad e -> M.return (Bad e)



    (* utilities ***********************************************)

    let send_msg c msg =
      M.send c (Msg.to_string msg)

    let send_msg_back msg =
      M.con_id >>= fun c ->
      M.send c (Msg.to_string msg)

    let my_nick_opt =
      M.get_s =>
        function
        | Waiting (_, o_nick) -> o_nick
        | Logged_in n -> Some n

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



    (* DSL for defining message functionality *)

    let commands : (string, Msg.prefix -> string list -> unit msg_try)
                     Hashtbl.t
      = Hashtbl.create 100

    let define_command cmd
          ~args:get_args
          ?must_be_logged_in:(must_login=false)
          fn
      =
      let handler prefix params =
        match get_args cmd params with
        | Bad e -> bad e
        | Ok args ->
           if must_login then
             M.get_s >>= function
             | Waiting _ -> bad (ERR._NOTREGISTERED)
             | Logged_in _ -> fn args
           else
             fn args
      in
      Hashtbl.add commands cmd handler


    let none cmd =
      const (Ok ())

    let one cmd = function
      | p::_ -> Ok p
      | _ -> Bad (ERR._NEEDMOREPARAMS cmd)

    let two cmd = function
      | p::q::_ -> Ok (p, q)
      | _ -> Bad (ERR._NEEDMOREPARAMS cmd)

    let four cmd = function
      | p::q::r::s::_ -> Ok (p, q, r, s)
      | _ -> Bad (ERR._NEEDMOREPARAMS cmd)



    (* commands ******************************************)

    let _MOTD = [
        "375", Printf.sprintf "- %s Message of the day -" !server_name;
        "372", "- Henlo and welcome to my OCaml IRC server.";
      ]

    (** send the MOTD **)
    let send_motd targ =
      List.enum _MOTD
      /@ (fun (cmd, str) -> with_server_prefix (Msg.simple1 cmd str))
      |> MonadEx.iter (send_msg targ)

    (** log in if the username and nick name are both set,
        and nick is not in use. otherwise, set state to Waiting **)
    let try_log_in maybe_user_info maybe_nick =
      match (maybe_user_info, maybe_nick) with
      | (Some (user, real), Some nick) ->
         M.on_users (M.DB.user_exists ~nick:nick) >>= fun exists ->
         if exists then
           (* nick name in use *)
           M.put_s (Waiting (maybe_user_info, None))
           >> bad (ERR._NICKNAMEINUSE nick)
         else
           (* ok to log in! *)
           M.con_id >>= fun con ->
           let uinfo = {
               Database.user_name = user;
               Database.host_name = "unknown.host";
               Database.real_name = real;
               Database.nick_name = nick;
               Database.modes = [];
             }
           in
           M.on_users (M.DB.add_user ~nick:nick con (Some uinfo))
           >> M.put_s (Logged_in nick)
           >> send_motd con
           >> ok_

      (* TODO LATER: we have to fucking synchronize
         nickname access across the entire network ???? *)

      | _ ->
         M.put_s (Waiting (maybe_user_info, maybe_nick))
         >> ok_


    let () = begin

        (**[  command: QUIT  ]**)
        define_command "QUIT" ~args:none
          (fun () ->
            send_msg_back (Msg.simple1 "ERROR" "Bye")
            >> M.quit);

        (**[  command: USER  ]**)
        define_command "USER" ~args:four
          (fun (user, _, _, real) ->
           M.get_s >>= function
           | Waiting (None, cur_nick) ->
              try_log_in (Some (user, real)) cur_nick

           | _ ->
              bad ERR._ALREADYREGISTERED);

        (**[  command: NICK  ]**)
        define_command "NICK" ~args:one
          (fun nick ->
            if not (Msg_parse.nickname_is_valid nick) then
              bad (ERR._ERRONEOUSNICKNAME nick)
            else
              M.get_s >>= function
              | Waiting (cur_user, None) ->
                 try_log_in cur_user (Some nick)

              | _ -> (* TODO: allow nick change while logged in? *)
                 bad ERR._NICKCOLLISION);

        (**[  command: PRIVMSG  ]**)
        define_command "PRIVMSG" ~args:two
          ~must_be_logged_in:true
          (fun (target, what) ->
            bad (ERR._NOSUCHNICK target))

      end



    (* implementation ******************************************)

    (** initialize the child actor **)
    let init () = Waiting (None, None)

    (** process just a string input **)
    let rec recv s =
      match CharParser.parse Msg_parse.message s with
      | Bad _ -> MonadEx.nop (* ignore malformed messages *)
      | Ok m ->
         recv_msg m >>= function
         | Ok _ -> MonadEx.nop
         | Bad msg_of_nick ->
            my_nick_opt >>= fun o_nick ->
            let nick = Option.default "*" o_nick in
            let msg = with_server_prefix (msg_of_nick nick) in
            send_msg_back msg

    (** extract relevant information out of message **)
    and recv_msg msg =
      let cmd = msg.Msg.raw_cmd in
      let params = msg.Msg.raw_params in
      or_default_prefix msg.Msg.raw_pfx >>= fun prefix ->
      try
        Hashtbl.find commands cmd prefix params
      with Not_found ->
        bad (ERR._UNKNOWNCOMMAND cmd)

    (** handle the client disconnecting **)
    let discon =
      MonadEx.nop

  end
