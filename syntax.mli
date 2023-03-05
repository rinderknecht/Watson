(* 
  Syntax errors 
*)

type message = string

(* 
  The type [error] distinguishes the unclosed constructs.
*)

type error =
  Unclosed of Location.t * message * Location.t * message
| Other    of message * Location.t


exception Error of error
