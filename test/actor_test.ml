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

(* convenience assertions *)
let str_eq = assert_equal ~printer:(Printf.sprintf "%S")
let pfx_eq = assert_equal ~printer:(Printf.sprintf "%S" % Irc.Prefix.to_string)
let contains =
  assert_equal
    ~cmp:(fun exp real ->
      Result.catch (String.find real) exp
      |> Result.is_ok)
    ~pp_diff:(fun pp (exp, real) ->
      Format.fprintf pp "expected to find needle %S in haystack %S" exp real)

module MM = Mock.Mock_monad
module MA = Mock.A

let main =
  let open Irc in
  "actor" >:::
    [
      "Mock/send" >::
        begin fun _ ->
        str_eq "HELLO :world\r\n"
          (Mock.run_sent 6 (MM.send_msg 6 @@ Msg.simple "HELLO" ["world"]));
        str_eq "HELLO :world\r\n"
          (Mock.run_sent 0 (MA.send_msg_back @@ Msg.simple "HELLO" ["world"]));
        str_eq "421 * ABC :Unknown command\r\n"
          (Mock.run_sent 0 (MA.send_reply_back @@ Replies._UNKNOWNCOMMAND "ABC"));
        end;

      "Mock/server_prefix" >::
        begin fun _ ->
        pfx_eq (Prefix.of_nick "mockserv.agar.irc")
          (Mock.run_result MA.get_server_prefix);
        end;

      "unknown command" >::
        begin fun _ ->
        contains "421 * IDK :Unknown command\r\n"
          (Mock.run_sent 0 (MA.on_recieve @@ Irc.Msg.simple "IDK" []));
        end;
    ]
