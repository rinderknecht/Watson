
(*
 This module provides the basic facilities for using source code
 locations throughout an abstract data-type. In particular, it
 supplies the lexer and parser specification with location information
 for error reporting.
*)


(*
  The location type [t] allows to reference a bit of source text.
  Field [loc_start] is the location of the first character of the
  denoted text (the character 0 is the first of the file), and
  field [loc_end] is the location of character just after the bit
  of text. \\
  Both fields must be non negative.
*)

type t = 
 { 
  loc_start : Lexing.position; 
  loc_end   : Lexing.position
 }

(*
 The value [none] is a special (dummy) location of ghost pieces of
 text (ie. automatically generated pieces of text).
*)

let none = Lexing.dummy_pos

(*
 The call [lexeme_loc (lexbuf)] returns the location of the last lexed
 lexeme in ocamllex.
*)

let lexeme_loc (lexbuf) =
 { 
  loc_start = Lexing.lexeme_start_p (lexbuf);
  loc_end   = Lexing.lexeme_end_p (lexbuf)
 } 

(*
 The call [symbol_loc] returns the location of the text matching the
 current left-hand side of a grammar production in ocamlyacc.
*)

let symbol_loc () = 
 { 
  loc_start = Parsing.symbol_start (); 
  loc_end   = Parsing.symbol_end ()
 }

(*
 The call [rhs_loc (n)] returns the location of the text matching the
 nth item of the current left-hand side of a grammar production in 
 ocamlyacc.
*)

let rhs_loc (n) = 
 { 
  loc_start = Parsing.rhs_start (n); 
  loc_end   = Parsing.rhs_end (n)
 }


(*
  The call [print (loc) (f)] prints in [stdout] the location [loc] in file
  [f].
*)

let (msg_file, msg_line, msg_chars, msg_to, msg_colon, warn_head) =
  match Sys.os_type with
  | "MacOS" -> ("Check file \"", "\"; line ", "; characters ", " to ", "", "### ")
  | _ -> ("Check file \"", "\", line ", ", characters ", "-", ".", "")


let print (loc) ~filename:(fname) =
  begin
    Format.print_string (msg_file); Format.print_string (fname);
    if   loc = none
    then ()
    else let rel_loc = new Linenum.relative_loc (fname) (loc.loc_start)
         in begin
              Format.print_string (msg_line); 
              Format.print_int (rel_loc#line_num);
              Format.print_string (msg_chars); 
              Format.print_int (loc.loc_start - rel_loc#line_start);
              Format.print_string (msg_to); 
              Format.print_int (loc.loc_end - rel_loc#line_start);
              Format.print_string (msg_colon);
              Format.force_newline ()
            end
  end

let to_string (loc) ~filename:(fname) =
  let rel_loc = new Linenum.relative_loc (fname) (loc.loc_start)
in "<l" ^ string_of_int (rel_loc#line_num) 
   ^ ",c" ^ string_of_int (loc.loc_start - rel_loc#line_start) 
   ^ "-" ^ string_of_int (loc.loc_end - rel_loc#line_start) ^ ">"


type line = int
type first_char_in_line = int
type last_char_in_line = int

let translate ~filename:(fname) (loc) =
  let rel_loc = new Linenum.relative_loc (fname) (loc.loc_start)
in (rel_loc#line_num, 
    loc.loc_start - rel_loc#line_start, 
    loc.loc_end - rel_loc#line_start)
