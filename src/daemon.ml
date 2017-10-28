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


(** create a daemon, given a child implementation **)
module Make(HF : Child.FUNC) = struct

  (*
- listen thread
- send pool
- recv thread(s)
- send thread(s)
   *)

  type server = {
      sr_user_db : DB.user_db;
      sr_user_db_lock : CU.lock;
    }


  (* child monad interface *****************************)

  type child = {
      mutable ch_st : Child.st;
      ch_user_db : DB.user_db;
      ch_user_db_lock : CU.lock;
      ch_con_id : int;
      ch_host_name : string;
    }

  module Child_monad = struct
    exception Quit

    type 'a t = child -> 'a
    module DB = DB

    let return x ch = x
    let bind f g ch = g (f ch) ch
    let map g f ch = g (f ch)

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
      Printf.printf "# con #%d is sending %S to con #%d\n"
        ch.ch_con_id str dst

    let quit ch =
      Printf.printf "# con #%d is quitting\n"
        ch.ch_con_id;
      raise Quit

  end

  module H = HF(Child_monad)



  (* server thread (main thread) ********************************)
  let run () =

    (* init server state *)
    let srv = {
        sr_user_db = DB.create_user_db !Config.initial_user_db_size;
        sr_user_db_lock = CU.create_lock ();
      }
    in

    (* server socket *)
    let srv_fd = Unix.socket
                   Unix.PF_INET
                   Unix.SOCK_STREAM
                   0
    in

    let srv_bind () =
       Unix.bind srv_fd (Unix.ADDR_INET (Unix.inet_addr_any, !Config.port));
       Unix.listen srv_fd !Config.max_pending_req
    in

    let srv_teardown () =
      Printf.printf "# tearing down ...\n";
      IO.flush IO.stdout;
      Unix.shutdown srv_fd Unix.SHUTDOWN_ALL;
      exit 0
    in

    let srv_accept () =
      Unix.accept srv_fd
    in

    (* bind + install ^C handler *)
    (try
       srv_bind ();

       Sys.set_signal Sys.sigint
         (Sys.Signal_handle
            (fun _ ->
              Printf.printf "\n# recieved ^C\n";
              IO.flush IO.stdout;
              srv_teardown ()));

       Printf.printf "# listening, port=%d\n" !Config.port;
       IO.flush IO.stdout;

     with Unix.Unix_error (e, blame_fn, blame_arg) ->
       raise (Failure ("failed to init server: " ^ Unix.error_message e)));

    (* accept loop *)
    while true do
      let con_fd, con_addr = srv_accept () in

      (*
        TODO: notify task pool
        TODO: spawn child thread
       *)
      Unix.shutdown con_fd Unix.SHUTDOWN_ALL
    done

end
