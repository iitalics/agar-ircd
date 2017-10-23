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

  type t = nick_name -> Msg.t

  let _UNKNOWNCOMMAND c nic = Msg.simple "421" [nic; c; "Unknown command"]
  let _NEEDMOREPARAMS c nic = Msg.simple "461" [nic; c; "Not enough parameters"]
  let _ALREADYREGISTERED nic = Msg.simple "462" [nic; "You may not register"]
  let _ERRONEOUSNICKNAME bad_nic nic = Msg.simple "432" [nic; bad_nic; "Erroneous nickname"]

end


module Make : FUNC =
  functor(M : MONAD) -> struct
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

    (* let params_2 cmd = function
      | p1::p2::_ -> ok (p1, p2)
      | [] -> bad (ERR._NEEDMOREPARAMS cmd) *)



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
         params_1 "NICK" params >>=? fun nick ->
         nick_avail nick >>=? fun nick ->
         (match st with
          | Waiting_nick ->
             M.put_s (Waiting_user nick)
             >> ok_
          | _ ->
             bad (ERR._ERRONEOUSNICKNAME nick))

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
