open BatteriesThread
open Batteries

module CLI() = struct

  module Child_monad = struct
    exception Quit

    module DB = Database.Hash_DB

    type 'a t = unit -> 'a
    let state = ref None
    let user_db = DB.create_user_db 6

    let bind m f () = f (m ()) ()
    let return = const
    let map = (%)

    let get_con () = 0
    let get_host () = "tty.repl"
    let get_st () = Ref.oget_exn state
    let put_st s () = state := Some s
    let on_users f () = f user_db
    let mut_users f () = f user_db

    let quit () = raise Quit
    let send i s () =
      (if i <> 0 then Printf.printf "# sending to con #%d:\n" i);
      print_string s;
      flush IO.stdout

    let run f = f ()
  end

  let run () =
    let module M = Child_monad in
    let module H = Child.Make(M) in

    Printf.printf "# running CLI \"server\"\n";

    Irc_common.server_name := "cmdline.test.irc";
    M.state := Some (H.init ());
    try
      while true do
        let line = read_line () ^ "\r\n" in
        M.run (H.recv line)
      done
    with
    | M.Quit -> print_string "quit.\n"
    | End_of_file -> ()

end


let _ =
  Daemon.Config.port := 6669;
  Daemon.Config.max_pending_req := 16;
  Daemon.run ()
