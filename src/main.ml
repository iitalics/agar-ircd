open BatteriesThread
open Batteries

module Console = struct
  exception Quit

  module DB = Database.Hash_DB

  type 'a t = unit -> 'a
  let state = ref None
  let user_db = DB.create_user_db ()

  let bind m f () = f (m ()) ()
  let return = const
  let map = (%)

  let get_con () = 0
  let get_host () = "tty.repl"
  let get_s () = Ref.oget_exn state
  let put_s s () = state := Some s
  let on_users f () = f user_db

  let quit () = raise Quit
  let send i s () =
    (if i <> 0 then Printf.printf "sending to con #%d:\n" i);
    print_string s;
    flush IO.stdout

  let run f = f ()
end



let _ =
  let module C = Console in
  let module H = Child.Make(C) in

  Irc_common.server_name := "cmdline.test.irc";
  C.state := Some (H.init ());
  try
    while true do
      let line = read_line () ^ "\r\n" in
      C.run (H.recv line)
    done
  with
  | C.Quit -> print_string "quit.\n"
  | End_of_file -> ()
