open Batteries
open OUnit2
module DB = Database
module ConMap = Map.Make(DB.Con)

module Mock = struct

  type st =
    { users : DB.Users.t;
      guests : DB.Guests.t;
      sent : Text.t ConMap.t }

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

    let mut_users f st = (), { st with users = f st.users }
    let mut_guests f st = (), { st with guests = f st.guests }

    let seq (ms : 'a t list) =
      List.fold_right
        (fun m m' -> bind m (fun _ -> m'))
        ms
        (fun st -> (), st)

  end

  module A = Actor.Impl(Mock_monad)

  (** [run_result m] runs monad [m], returning the
      resulting value. *)
  let run_result
        ?(guests=DB.Guests.empty)
        ?(users=DB.Users.empty)
        (m : 'a Mock_monad.t) =
    fst (m { guests = guests;
             users = users;
             sent = ConMap.empty })

  (** [run_state m] runs monad [m], returning the
      state after the monad is run. *)
  let run_state
        ?(guests=DB.Guests.empty)
        ?(users=DB.Users.empty)
        (m : 'a Mock_monad.t) =
    snd (m { guests = guests;
             users = users;
             sent = ConMap.empty })

  (** [run_sent c m] runs monad [m], returning all
      of the data sent to connection [c]. *)
  let run_sent
        ?(guests=DB.Guests.empty)
        ?(users=DB.Users.empty)
        ?(targ=0)
        (m : 'a Mock_monad.t) =
    let sent = (snd (m { guests = guests;
                         users = users;
                         sent = ConMap.empty })).sent in
    try
      Text.to_string (ConMap.find targ sent)
    with Not_found -> ""

end

(* pretty printers *)
let qq = Printf.sprintf "%S"
let pp_opt f = function
  | None -> "None"
  | Some x -> "Some(" ^ f x ^ ")"

(* convenience assertions *)
let str_eq = assert_equal ~printer:qq
let pfx_eq = assert_equal ~printer:(qq % Irc.Prefix.to_string)
let contains =
  assert_equal
    ~cmp:(fun exp real ->
      Result.catch (String.find real) exp
      |> Result.is_ok)
    ~pp_diff:(fun pp (exp, real) ->
      Format.fprintf pp "expected to find needle %S in haystack %S" exp real)
let contains_all ns h =
  List.iter (fun n -> contains n h) ns

(* objects for use in tests *)
let lain = { DB.uent_con = 0;
             DB.uent_nick = "lain";
             DB.uent_host = DB.Host_there ("wired", 1) }

let just_lain =
  DB.Users.insert lain DB.Users.empty


module MM = Mock.Mock_monad
module MA = Mock.A

let main =
  let open Irc in
  "actor" >:::
    [
      "Mock/send" >::
        begin fun _ ->
        str_eq "HELLO :world\r\n"
          (Mock.run_sent ~targ:6 (MM.send_msg 6 @@ Msg.simple "HELLO" ["world"]));
        str_eq "HELLO :world\r\n"
          (Mock.run_sent (MA.send_msg_back @@ Msg.simple "HELLO" ["world"]));
        str_eq "421 * ABC :Unknown command\r\n"
          (Mock.run_sent (MA.send_reply_back @@ Replies._UNKNOWNCOMMAND "ABC"));
        end;

      "Mock/server_prefix" >::
        begin fun _ ->
        pfx_eq (Prefix.of_nick "mockserv.agar.irc")
          (Mock.run_result MA.get_server_prefix);
        end;

      "Mock/on_init,on_quit" >::
        begin fun _ ->
        let st1 = Mock.run_state (MA.on_init ()) in
        assert_bool "must contain guest entry for con #0"
          (Option.is_some @@ DB.Guests.by_con 0 st1.Mock.guests);

        let st2 = Mock.run_state (MM.seq [MA.on_init (); MA.on_quit ()]) in
        assert_bool "must not contain entry for con #0"
          (Option.is_none @@ DB.Guests.by_con 0 st2.Mock.guests);

        let st3 = Mock.run_state
                    ~users:just_lain
                    (MA.on_quit ()) in
        assert_bool "must not contain user entry for con #0"
          (Option.is_none @@ DB.Users.by_con 0 st3.Mock.users);


        end;

      "unknown command" >::
        begin fun _ ->
        contains "421 * IDK :Unknown command\r\n"
          (Mock.run_sent (MA.on_recieve @@ Msg.simple "IDK" []));
        end;

      "command:CAP" >::
        begin fun _ ->
        contains_all ["CAP * LS :\r\n";
                      "CAP * LIST :\r\n";
                      "410 * FOO :Invalid CAP command\r\n"]
          (Mock.run_sent
             (MM.seq [MA.on_recieve @@ Msg.simple1 "CAP" "LS";
                      MA.on_recieve @@ Msg.simple1 "CAP" "LIST";
                      MA.on_recieve @@ Msg.simple1 "CAP" "FOO"]));
        end;

      "command:USER" >::
        begin fun _ ->
        let msg1 = Msg.simple "USER" ["foo"; "*"; "*"; "Foo"] in
        let msg2 = Msg.simple "USER" ["lain"; "*"; "*"; "Lain"] in
        let st1 = Mock.run_state
                    (MM.seq [MA.on_init();
                             MA.on_recieve msg1;
                             MA.on_recieve msg2])
        in
        begin match DB.Guests.by_con 0 st1.Mock.guests with
        | Some lain ->
           assert_equal ~printer:(pp_opt qq) (Some "lain")
             lain.DB.gent_user;
           assert_equal ~printer:(pp_opt qq) (Some "Lain")
             lain.DB.gent_real;
           assert_equal ~printer:(pp_opt qq) None
             lain.DB.gent_nick;
        | None ->
           assert_failure "no guest entry"
        end;

        let msg3 = Msg.simple "USER" ["arisu"; "*"; "*"; "Arisu"] in
        contains "462 lain :You may not reregister"
          (Mock.run_sent
             ~users:just_lain
             (MA.on_recieve msg3));

        end;
    ]
