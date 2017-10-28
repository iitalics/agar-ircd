open BatteriesThread
open Batteries


type lock = Concurrent.lock

let create_lock
      ?(enter=identity)
      ?(leave=identity)
      () =
  Concurrent.create enter leave

let with_lock = Concurrent.sync
