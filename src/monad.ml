open Batteries

(**
    Signature for monads.
 *)
module type SIG = sig
  type 'a t
  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(**
    Extra monad utilities, such as infix operators.
*)
module Extra(M : SIG) = struct

  let ( >>= ) = M.bind
  let ( >> ) a b = a >>= fun _ -> b
  let ( => ) f m = M.map m f

  (** no-op computation with no result *)
  let pure_ = M.pure ()

  (** ignore result of computation *)
  let void m = m >> pure_

  (** iterate through a list, carrying monadic
      effects for each element. *)
  let rec m_iter f = function
    | [] -> M.pure ()
    | x::xs -> f x >>= fun _ -> m_iter f xs

end

(**
    Functions for monad-like results within another monad.
*)
module Result(M : SIG) = struct

  let ok x = M.pure (Ok x)
  let bad y = M.pure (Bad y)

  let ( >>=? ) mr f =
    M.bind mr
      (function
       | Ok x -> f x
       | Bad b -> M.pure (Bad b))

  let ( >>? ) mr m' =
    M.bind mr
      (function
       | Ok _ -> m'
       | Bad b -> M.pure (Bad b))

end
