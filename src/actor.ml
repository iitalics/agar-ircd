open Batteries
module DB = Database

(**
    Actor-capable monad extension
 *)
module type MONAD = sig
  include Monad.SIG

  val get_parent_con : DB.con option t
  val get_con : DB.con t
  val send_msg : DB.con -> Irc.msg -> unit t
  (* val should_close : bool -> unit t *)

  (* val with_users : ?mut:bool -> (DB.users -> 'a) -> 'a t *)
  (* val with_guests : ?mut:bool -> (DB.guests -> 'a) -> 'a t *)

end

(**
    Signature for actor implementations
 *)
module type IMPL =
  functor(M : MONAD) -> sig

    val on_init : unit -> unit M.t
    val on_quit : unit -> unit M.t
    val on_recieve : Irc.msg -> unit M.t

  end


(**
   Implementation of actor
 *)
module Impl(M : MONAD) = struct
  module Ex = Monad.Extra(M)
  open Ex

  let on_init () = pure_
  let on_quit () = pure_
  let on_recieve _ = pure_

end
