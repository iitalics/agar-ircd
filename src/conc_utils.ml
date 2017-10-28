open BatteriesThread
open Batteries


(* locks ****)

type lock = Concurrent.lock

let create_lock
      ?(enter=identity)
      ?(leave=identity)
      () =
  Concurrent.create enter leave

let with_lock = Concurrent.sync


(* channels ****)

type 'a chan = 'a CCBlockingQueue.t

let create_chan
      ?(size=0)
      () =
  CCBlockingQueue.create size

let chan_get =
  CCBlockingQueue.take

let chan_get_opt =
  CCBlockingQueue.try_take

let chan_put x c =
  CCBlockingQueue.push c x
