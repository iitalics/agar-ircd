open Batteries
module DB = Database

(**
    Actor-capable monad extension
 *)
module type MONAD = sig
  include Monad.SIG

  (* val get_parent_con : DB.con option t *)
  val get_con : DB.con t
  (* val send_msg : DB.con -> Irc.msg -> unit t *)
  (* val should_close : bool -> unit t *)

  (* val with_users : ?mut:bool -> (DB.users -> 'a) -> 'a t *)
  (* val with_guests : ?mut:bool -> (DB.guests -> 'a) -> 'a t *)

end

(**
    Signature for actor implementations
 *)
module type IMPL =
  functor(M : MONAD) -> sig

    type st

    val on_init : unit -> st M.t
    val on_quit : st -> unit M.t
    val on_recieve : st -> Irc.msg -> st M.t

  end


(**
   Implementation of actor
 *)
module Impl(M : MONAD) = struct
  module Ex = Monad.Extra(M)
  open Ex

  type st
    = Guest | User

  let on_init () = M.pure Guest
  let on_quit st = pure_
  let on_recieve st _ = M.pure st

end
