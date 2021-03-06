open Batteries
open Irc_common

type st
  = Waiting of (user_name * string) option * nick_name option
  | Logged_in of nick_name

module type MONAD = sig
  include Monad.SIG

  module DB : Database.SIG

  (** gets the child's state **)
  val get_st : st t
  (** sets the child's state **)
  val put_st : st -> unit t

  (** applies the given immutable operation to the users database **)
  val on_users : (DB.user_db -> 'a) -> 'a t
  (** applies the given mutable operation to the users database **)
  val mut_users : (DB.user_db -> unit) -> unit t

  (** the connection ID of this child's connection **)
  val get_con : int t
  (** the hostname string of this child's connection **)
  val get_host : string t

  (** sends a string to the given connection ID **)
  val send : int -> string -> unit t
  (** closes the connection **)
  val quit : 'a t

end


module type FUNC =
  functor(M : MONAD) -> sig

    (** get a new initial state **)
    val init : unit -> st
    (** callback when a child recieves a message (CR LF terminated string) **)
    val recv : string -> unit M.t
    (** callback when the connection is disconnected **)
    val discon : unit M.t

  end


module ERR = struct

  type t = nick_name -> Msg.t

  let _UNKNOWNCOMMAND cmd nic = Msg.simple "421" [nic; cmd; "Unknown command"]
  let _NEEDMOREPARAMS cmd nic = Msg.simple "461" [nic; cmd; "Not enough parameters"]
  let _NOTREGISTERED nic = Msg.simple "451" [nic; "You have not registered"]

  let _ALREADYREGISTERED nic = Msg.simple "462" [nic; "You may not reregister"]
  let _NONICKNAMEGIVEN nic = Msg.simple "431" [nic; "No nickname given"]
  let _ERRONEOUSNICKNAME bad_nic nic = Msg.simple "432" [nic; bad_nic; "Erroneous nickname"]
  let _NICKNAMEINUSE bad_nic nic = Msg.simple "433" [nic; bad_nic; "Nickname is already in use"]
  let _NICKCOLLISION nic = Msg.simple "432" [nic; nic; "Nickname collision"]

  let _NOSUCHNICK bad_nic nic = Msg.simple "401" [nic; bad_nic; "No such nick/channel"]
  let _NOSUCHCHANNEL chan nic = Msg.simple "401" [nic; chan; "No such channel"]

end


module Make : FUNC =
  functor(M : MONAD) -> struct
    module DB = M.DB
    module MonadEx = Monad.Extras(M)
    module Infix = Monad.Infix(M)
    open Infix


    (* result-carrying monad *)

    type 'a msg_try = ('a, ERR.t) result M.t

    let ok_ : unit msg_try        = M.return (Ok ())
    let ok : 'a -> 'a msg_try     = fun x -> M.return (Ok x)
    let bad : ERR.t -> 'a msg_try = fun y ->  M.return (Bad y)

    let ( >>=? ) : 'a msg_try -> ('a -> 'b msg_try) -> 'b msg_try =
      fun m f ->
      m >>= function
      | Ok x -> f x
      | Bad e -> M.return (Bad e)

    let rec try_map_list f = function
      | [] -> ok []
      | x::xs ->
         f x >>=? fun y ->
         try_map_list f xs >>=? fun ys ->
         ok (y::ys)



    (* utilities ***********************************************)

    let with_server_prefix msg =
      Msg.with_prefix
        (Msg.Prefix_server !server_name)
        msg

    let send_msg c msg =
      M.send c (Msg.to_string msg)

    let send_msg_back msg =
      M.get_con >>= fun c ->
      M.send c (Msg.to_string msg)

    let get_nick_opt =
      M.get_st =>
        function
        | Waiting (_, o_nick) -> o_nick
        | Logged_in n -> Some n


    let default_prefix =
      M.get_host >>= fun host ->
      get_nick_opt >>= function
      | None ->
         M.return (Msg.Prefix_user ("*", None, Some host))
      | Some nick ->
         M.on_users (DB.user_info ~nick:nick) >>= fun maybe_info ->
         let maybe_user =
           Option.map (fun i -> i.Database.user_name)
             maybe_info
         in
         M.return (Msg.Prefix_user (nick, maybe_user, Some host))

    let or_default_prefix = function
      | Some pfx -> M.return pfx
      | None -> default_prefix



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
             M.get_st >>= function
             | Waiting _ -> bad (ERR._NOTREGISTERED)
             | Logged_in _ -> fn prefix args
           else
             fn prefix args
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

    let motd_list_for nick =
      [
        "001", [Printf.sprintf "Welcome to the Internet Relay Network %s" nick];
        "002", [Printf.sprintf "Your host is %s, running %s" !server_name server_version];
        "003", [Printf.sprintf "This server was created %s" server_date];
        "004", [!server_name; server_version; "iosw"; "mov"];
        "375", [Printf.sprintf "- %s Message of the day -" !server_name];
        "372", ["- Henlo and welcome to my OCaml IRC server."];
      ]

    (** send the MOTD **)
    let send_motd user_info targ =
      let pfx = Some (Msg.Prefix_server !server_name) in
      let nick = user_info.Database.nick_name in
      List.enum (motd_list_for nick)
      |> Enum.map (fun (cmd, params) -> {
                       Msg.raw_pfx = pfx;
                       Msg.raw_cmd = cmd;
                       Msg.raw_params = params })
      |> MonadEx.iter (send_msg targ)

    (** log in if the username and nick name are both set,
        and nick is not in use. otherwise, set state to Waiting **)
    let try_log_in maybe_user_real maybe_nick =
      match maybe_user_real, maybe_nick with
      | Some (user, real), Some nick ->
         M.on_users (DB.user_exists ~nick:nick) >>= fun exists ->
         if exists then
           (* nick name in use *)
           M.put_st (Waiting (maybe_user_real, None))
           >> bad (ERR._NICKNAMEINUSE nick)
         else
           (* ok to log in! *)
           M.get_con >>= fun con ->
           M.get_host >>= fun host ->
           let user_info = {
               Database.user_name = user;
               Database.host_name = host;
               Database.real_name = real;
               Database.nick_name = nick;
               Database.modes = [] }
           in
           M.mut_users (DB.add_user ~nick:nick con (Some user_info))
           >> M.put_st (Logged_in nick)
           >> send_motd user_info con
           >> ok_

      (* TODO LATER: we have to fucking synchronize
         nickname access across the entire network ???? *)

      | _ ->
         M.put_st (Waiting (maybe_user_real, maybe_nick))
         >> ok_


    let () = begin

        (**[  command: QUIT  ]**)
        define_command "QUIT" ~args:none
          (fun _ () ->
            send_msg_back (Msg.simple1 "ERROR" "Bye")
            >> M.quit);


        (**[  command: USER  ]**)
        define_command "USER" ~args:four
          (fun _ (user, _, _, real) ->
           M.get_st >>= function
           | Waiting (None, cur_nick) ->
              try_log_in (Some (user, real)) cur_nick
           | _ ->
              bad ERR._ALREADYREGISTERED);


        (**[  command: NICK  ]**)
        define_command "NICK" ~args:one
          (fun _ nick ->
            if String.is_empty nick then
              bad ERR._NONICKNAMEGIVEN
            else if not (Msg_parse.nickname_is_valid nick) then
              bad (ERR._ERRONEOUSNICKNAME nick)
            else
              M.get_st >>= function
              | Waiting (cur_user, None) ->
                 try_log_in cur_user (Some nick)
              | _ -> (* TODO: allow nick change while logged in? *)
                 bad ERR._NICKCOLLISION);


        (**[  command: PRIVMSG  ]**)
        define_command "PRIVMSG" ~args:two
          ~must_be_logged_in:true
          (fun prefix (who, what) ->
            (* convert target names into routes *)
            String.split_on_char ',' who
            |> try_map_list (fun name ->
                   (* TODO: send to channels if name begins with # *)
                   M.on_users (DB.user_route ~nick:name) >>= function
                   | Some con ->
                      ok (con, name)
                   | None ->
                      bad (ERR._NOSUCHNICK name))

            >>= function
            | Bad b -> bad b
            | Ok targs ->
               (* send to all recipients *)
               List.enum targs
               |> MonadEx.iter (fun (con, name) ->
                      send_msg con {
                          Msg.raw_pfx = Some prefix;
                          Msg.raw_cmd = "PRIVMSG";
                          Msg.raw_params = [name; what] })
               >> ok_);

      end



    (* implementation ******************************************)

    (** initialize the child actor **)
    let init () = Waiting (None, None)

    (** process string input **)
    let rec recv s =
      match CharParser.parse Msg_parse.message s with
      | Bad _ -> MonadEx.nop (* ignore malformed messages *)
      | Ok m ->
         recv_msg m >>= function
         | Ok _ -> MonadEx.nop
         | Bad msg_of_nick ->
            get_nick_opt >>= fun o_nick ->
            let nick = Option.default "*" o_nick in
            let msg = with_server_prefix (msg_of_nick nick) in
            send_msg_back msg

    (** dispatch to command handler **)
    and recv_msg msg =
      let cmd = msg.Msg.raw_cmd in
      let params = msg.Msg.raw_params in
      (* or_default_prefix msg.Msg.raw_pfx >>= fun prefix -> *)
      (*   -- for now, ignore given prefix; we can't trust it until
              we support multiple nodes + oper status etc. *)
      default_prefix >>= fun prefix ->
      try
        Hashtbl.find commands cmd prefix params
      with Not_found ->
        bad (ERR._UNKNOWNCOMMAND cmd)

    (** handle the client disconnecting **)
    let discon =
      M.get_st >>= function
      | Logged_in nick ->
         M.mut_users (DB.del_user ~nick:nick)
         >> MonadEx.nop

      | _ -> MonadEx.nop

  end
