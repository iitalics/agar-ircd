open BatteriesThread
open Batteries
module CU = Conc_utils
module DB = Database.Hash_DB

(** daemon configuration **)
module Config = struct
  let port = ref 6669
  let max_pending_req = ref 8
  let initial_user_db_size = ref 200
end

module Logger : sig
  type output
  val fmt : ('a, output, unit) format -> 'a
end = struct
  type output = unit IO.output
  let fmt s = Printf.kfprintf IO.flush IO.stdout s
end

(** create a daemon, given a child implementation **)
module Make(HF : Child.FUNC) = struct

  (*
- listen thread
- send pool
- recv thread(s)
- send thread(s)
   *)


  (* child monad interface *****************************)

  type child = {
      mutable ch_st : Child.st;
      ch_user_db : DB.user_db;
      ch_user_db_lock : CU.lock;
      ch_con_id : int;
      ch_host_name : string;
    }

  module Child_monad : Child.MONAD = struct
    exception Quit

    type 'a t = child -> 'a
    module DB = DB

    let bind f g ch = g (f ch) ch
    let return = const
    let map = (%)

    let get_st ch = ch.ch_st
    let put_st s ch = ch.ch_st <- s

    let on_users f ch =
      CU.with_lock ch.ch_user_db_lock
        f ch.ch_user_db
    let mut_users f ch =
      CU.with_lock ch.ch_user_db_lock
        f ch.ch_user_db

    let get_con ch = ch.ch_con_id
    let get_host ch = ch.ch_host_name

    let send dst str ch =
      Logger.fmt "# con #%d is sending %S to con #%d\n"
        ch.ch_con_id str dst

    let quit ch =
      Logger.fmt "# con #%d is quitting\n"
        ch.ch_con_id;
      raise Quit

  end

  module H = HF(Child_monad)


  (* server object *************************************************)

  type server = {
      sr_user_db : DB.user_db;
      sr_user_db_lock : CU.lock;
      sr_fd : Unix.file_descr;
    }

  (** attempt to initialize new server **)
  let create () =
    try
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, !Config.port));
      Unix.listen fd !Config.max_pending_req;
      let sr = {
          sr_user_db = DB.create_user_db !Config.initial_user_db_size;
          sr_user_db_lock = CU.create_lock ();
          sr_fd = fd
        }
      in
      Logger.fmt "# listening, port=%d\n" !Config.port;
      Some sr
    with e ->
      Logger.fmt "# error starting server: %s\n"
        (Printexc.to_string e);
      None

  (** tears down server socket **)
  let teardown sr =
    Logger.fmt "# tearing down ...\n";
    Unix.shutdown sr.sr_fd Unix.SHUTDOWN_ALL

  (** accepts a new client (blocking), spawning the
      necessary threads etc. **)
  let accept sr =
    let con_fd, con_adr = Unix.accept sr.sr_fd in
    Unix.shutdown con_fd Unix.SHUTDOWN_ALL

end



(* entry point *)
let run () =
  let module D = Make(Child.Make) in
  match D.create () with
  | None -> ()
  | Some sr ->
     try
       while true do
         Sys.catch_break true;
         D.accept sr
       done
     with Sys.Break ->
       D.teardown sr
