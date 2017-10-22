open Batteries

module type MONAD = sig
  include Monad.SIG

  val con_id : int t
  val quit : 'a t
  val send : Routing.target -> string -> unit t

end


module type FUNC =
  functor(M : MONAD) -> sig

    val init : unit M.t
    val recv : string -> unit M.t
    val discon : unit M.t

  end


module Make : FUNC =
  functor(M : MONAD) -> struct
    module Infix = Monad.Infix(M)
    open Infix


    let init =
      M.return ()

    let send_back s =
      M.con_id >>= fun i ->
      M.send (Routing.To_con i) s

    let recv s =
      (match CharParser.parse Msg_parse.message s with
        | Bad _ ->
           M.return {
               Msg.raw_pfx = None;
               Msg.raw_cmd = "421";
               Msg.raw_params = ["Unknown command"] }

        | Ok m ->
           if m.Msg.raw_cmd = "QUIT" then
             M.quit
           else
             M.return m)

      >>= fun m ->
      let m' = Msg.with_prefix (Msg.Prefix_server "irc.node") m in
      send_back (Msg.to_string m' ^ "\r\n")


    let discon =
      M.return ()

  end
