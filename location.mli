
(*
 This module provides the basic facilities for using source code
 locations throughout an abstract data-type. In particular, it
 supplies the lexer and parser specification with location information
 for error reporting.
*)


(*
 The location type
*)

type t

(*
 The value [none] is a special (dummy) location of ghost pieces of
 text (ie. automatically generated pieces of text).
*)

val none : t

(*
 The call [lexeme_loc (lexbuf)] returns the location of the last lexed
 lexeme in ocamllex.
*)

val lexeme_loc : Lexing.lexbuf -> t

(*
 The call [symbol_loc] returns the location of the text matching the
 current left-hand side of a grammar production in ocamlyacc.
*)

val symbol_loc : unit -> t

(*
 The call [rhs_loc (n)] returns the location of the text matching the
 nth item of the current left-hand side of a grammar production in 
 ocamlyacc.
*)

val rhs_loc : int -> t

(*
  The call [print loc f] prints in [stdout] the location [loc] in file
  [f].
*)

val print : t -> filename:string -> unit


(* 
   The following declarations are for debugging purposes only. 
*)

val to_string : t -> filename:string -> string 

(*
  The call [translate f l] returns a triple whose first component is
  the line number corresponding to the location [l] in file [f],
  the second component is the position of the first character corresponding
  to the location [l] in file [f] (the character 0 is the first on a line),
  and the third and last component is the position of the last character
  corresponding to the location [l] in file [f].
*)

type line = int
type first_char_in_line = int
type last_char_in_line = int

val translate : filename:string -> t -> line * first_char_in_line * last_char_in_line


