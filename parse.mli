(* Wrapper for module Parser *)

type tracing_mode =
  Tracing_to of string
| No_tracing

val grammar : ?parsing:tracing_mode -> ?lexing:tracing_mode -> string -> Core.AST.t
