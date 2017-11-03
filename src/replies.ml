open Batteries
open Irc

type reply = nick_name -> msg

let _UNKNOWNCOMMAND cmd = Msg.reply 421 [cmd;"Unknown command"]
let _NEEDMOREPARAMS cmd = Msg.reply 461 [cmd; "Not enough parameters"]
let _ALREADYREGISTERED = Msg.reply 462 ["You may not reregister"]
