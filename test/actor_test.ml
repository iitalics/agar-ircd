open Batteries
open OUnit2
module DB = Database
module ConMap = Map.Make(DB.Con)

module Mock = struct

  type st =
    { users : DB.Users.t;
      guests : DB.Guests.t;
      sent : Text.t ConMap.t }

  let empty_state =
    { users = DB.Users.empty;
      guests = DB.Guests.empty;
      sent = ConMap.empty }

  (** mock monad for actors *)
  module Mock_monad = struct

    type 'a t = st -> 'a * st
    let pure x st = x, st
    let bind m f st = let x, st' = m st in f x st'
    let map f m st = let x, st' = m st in f x, st'

    let get_server_name st = "mockserv", st
    let get_parent_con st = None, st
    let get_con st = 0, st

    let send_msg c msg st =
      let msg_txt = Text.of_string (Irc.Msg.to_string msg ^ "\r\n") in
      (),
      { st with sent = ConMap.modify_def
                         Text.empty
                         c (fun txt -> Text.append txt msg_txt)
                         st.sent }

    let with_users f st = f st.users, st
    let with_guests f st = f st.guests, st

  end

  module A = Actor.Impl(Mock_monad)

  (** [run_result m] runs monad [m], returning the
      resulting value. *)
  let run_result (m : 'a Mock_monad.t) =
    fst (m empty_state)

  (** [run_state m] runs monad [m], returning the
      state after the monad is run. *)
  let run_state (m : 'a Mock_monad.t) =
    snd (m empty_state)

  (** [run_sent c m] runs monad [m], returning all
      of the data sent to connection [c]. *)
  let run_sent c (m : 'a Mock_monad.t) =
    let sent = (snd (m empty_state)).sent in
    try
      Text.to_string
        (ConMap.find c sent)
    with Not_found -> ""
end


let str_eq = assert_equal ~printer:(Printf.sprintf "%S")
let pfx_eq = assert_equal ~printer:(Printf.sprintf "%S" % Irc.Prefix.to_string)

module MA = Mock.A

let main =
  "actor" >:::
    [
      "Mock/server_prefix" >::
        begin fun _ ->
        pfx_eq (Irc.Prefix.of_nick "mockserv.agar.irc")
          (Mock.run_result MA.get_server_prefix);
        end;
    ]
