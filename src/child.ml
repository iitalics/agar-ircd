open Batteries

module type MONAD = sig
  include Monad.SIG

  val con_id : int t
  val quit : 'a t
  val send : Routing.target -> string -> unit t

end


module type FUNC =
  functor(M : MONAD) -> sig

    val init : unit M.t
    val recv : string -> unit M.t
    val discon : unit M.t

  end


module Make =
  functor(M : MONAD) -> struct
    module Infix = Monad.Infix(M)
    open Infix


    let init =
      M.return ()

    let recv s =
      M.con_id >>= fun i ->
      M.send (Routing.To_con i) s

    let discon =
      M.return ()

  end
