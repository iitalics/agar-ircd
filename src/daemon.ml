open BatteriesThread
open Batteries
module CU = Conc_utils
module DB = Database.Hash_DB

(** daemon configuration **)
module Config = struct
  let port = ref 6669
  let max_pending_req = ref 8
  let task_buffer_size = ref 16
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

  type task
    = Task_send of int * string
    | Task_spawn of Unix.file_descr * (int -> unit)
    | Task_kill of int

  type server = {
      sr_user_db : DB.user_db;
      sr_user_db_lock : CU.lock;
      sr_fd : Unix.file_descr;
      sr_junction_tasks : task CU.chan;
      sr_junction_thread : Thread.t;
    }

  type child = {
      mutable ch_st : Child.st;
      ch_user_db : DB.user_db;
      ch_user_db_lock : CU.lock;
      ch_con_id : int;
      ch_host_name : string;
    }


  (* junction thread *****************************)

  (** spawn a thread that handles task from
      the returned channel **)
  let spawn_junction () =
    let module Map = CCIntMap in

    let next_id = ref 0 in
    let tasks = CU.create_chan ~size:!Config.task_buffer_size () in

    let rec loop thds =
      let thds' = match CU.chan_get tasks with
        | Task_send (con_id, msg) ->
           (match Map.find con_id thds with
            | None ->
               Logger.fmt "# attempted to send to nonexistant con #%d\n" con_id

            | Some (thd, fd) ->
               let bs = Bytes.of_string msg in
               ignore (Unix.send fd bs 0 (Bytes.length bs) []));
           thds

        | Task_spawn (fd, launch) ->
           let con_id = !next_id in
           next_id := con_id + 1;
           let thd = Thread.create launch con_id in
           Map.add con_id (thd, fd) thds

        | Task_kill con_id ->
           Map.find con_id thds
           |> Option.may (fun (thd, fd) ->
                  Thread.kill thd;
                  Unix.shutdown fd Unix.SHUTDOWN_ALL);
           Map.remove con_id thds
      in
      loop thds'
    in

    tasks, Thread.create loop Map.empty


  (* child monad interface *****************************)

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

  let create_child ~server:sr ~host_name con_id = {
      ch_st = H.init ();
      ch_user_db = sr.sr_user_db;
      ch_user_db_lock = sr.sr_user_db_lock;
      ch_con_id = con_id;
      ch_host_name = host_name;
    }


  (* server object *************************************************)

  (** attempt to initialize new server **)
  let create_server () =
    try
      (* create socket *)
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, !Config.port));
      Unix.listen fd !Config.max_pending_req;

      (* create junction *)
      let jn_tasks, jn_thd = spawn_junction () in

      (* create server *)
      let sr = {
          sr_user_db = DB.create_user_db !Config.initial_user_db_size;
          sr_user_db_lock = CU.create_lock ();
          sr_fd = fd;
          sr_junction_tasks = jn_tasks;
          sr_junction_thread = jn_thd;
        }
      in
      Logger.fmt "# listening, port=%d\n" !Config.port;
      Some sr
    with e ->
      Logger.fmt "# error starting server: %s\n"
        (Printexc.to_string e);
      None

  (** tears down server socket **)
  let server_teardown sr =
    Logger.fmt "# tearing down ...\n";
    Unix.shutdown sr.sr_fd Unix.SHUTDOWN_ALL

  (** accepts a new client (blocking), spawning the
      necessary threads etc. **)
  let server_accept sr =
    let con_fd, con_adr = Unix.accept sr.sr_fd in
    let host_name =
      match con_adr with
      | Unix.ADDR_UNIX s -> "unix:" ^ s
      | Unix.ADDR_INET (adr, port) -> Unix.string_of_inet_addr adr
    in
    Logger.fmt "# connection from %s\n" host_name;
    Unix.shutdown con_fd Unix.SHUTDOWN_ALL

end



(* entry point *)
let run () =
  let module D = Make(Child.Make) in
  match D.create_server () with
  | None -> ()
  | Some sr ->
     try
       while true do
         Sys.catch_break true;
         D.server_accept sr
       done
     with Sys.Break ->
       D.server_teardown sr
