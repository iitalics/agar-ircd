open Batteries
open OUnit2
module DB = Database

module Mock = struct
  module ConMap = Map.Make(Int)

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

    let get_parent_con st = None, st
    let get_con st = 0, st

    let send_msg c msg st =
      let msg_txt = Text.of_string (Irc.Msg.to_string msg) in
      (),
      { st with sent = ConMap.modify_def
                         Text.empty
                         c (fun txt -> Text.append txt msg_txt)
                         st.sent }

    let with_users f st =
      f st.users, st

    let with_guests f st =
      f st.guests, st

  end

  module A = Actor.Impl(Mock_monad)

end


let main =
  "actor" >:::
    [
    ]
