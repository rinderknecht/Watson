{
 (* This ocamllex specification is used to build a lexer for
    Extended-BNF grammars.
 *)


 let name = "Lexer"

 type message = string


 (* Fatal errors *)

 exception Fatal_error of message

 (* Comment depth *)

 let comment_depth = ref (0)

 (* Lexical errors *)

 type start_loc = int
 type end_loc = int

 type error =
   Illegal_keyword      of string * Location.t
 | Illegal_character    of string * Location.t
 | Keyword_expected     of Location.t
 | Unterminated_string  of Location.t
 | Unterminated_comment of Location.t

 type local_error =
   Local_unterminated_string
 | Local_unterminated_comment

 exception Error of error
 exception Locally_handled_error of local_error

 let handle_lexical_error (fn) (lexbuf) =
   let loc = Location.lexeme_loc (lexbuf)
 in try
      fn (lexbuf)
    with Locally_handled_error (kind) ->
           match kind with
             Local_unterminated_string ->
               raise (Error (Unterminated_string (loc)))
           | Local_unterminated_comment ->
               raise (Error (Unterminated_string (loc)))

 (* String processing *)

 let initial_string_buffer = String.create (256)
 let string_buff = ref (initial_string_buffer)
 let string_index = ref (0)

 let reset_string_buffer () =
   begin
     string_buff := initial_string_buffer;
     string_index := 0
   end

 let store_string_char (c) =
   if   !string_index >= String.length !string_buff 
   then begin
          let new_buff = String.create (String.length !string_buff * 2) 
          in String.blit !string_buff 0 new_buff 0 (String.length !string_buff);
             string_buff := new_buff
        end;
   !string_buff.[!string_index] <- c;
   incr string_index

 let get_stored_string () =
   String.sub (!string_buff) (0) (!string_index)


 (* To translate escape sequences *)

 let char_for_backslash =
   match Sys.os_type with
   | "Unix" | "Win32" ->
       begin function
       | 'n' -> '\010'
       | 'r' -> '\013'
       | 'b' -> '\008'
       | 't' -> '\009'
       | c   -> c
       end
   | "MacOS" ->
       begin function
       | 'n' -> '\013'
       | 'r' -> '\010'
       | 'b' -> '\008'
       | 't' -> '\009'
       | c   -> c
       end
   | x -> raise (Fatal_error ("Lexer.char_for_backslash: unknown system type \"" ^ x ^ "\"."))

 (* To translate decimal codes *)

 let char_for_decimal_code lexbuf (i) =
   Char.chr (100 * (Char.code (Lexing.lexeme_char lexbuf i) - 48) +
                 10 * (Char.code (Lexing.lexeme_char lexbuf (i+1)) - 48) +
                      (Char.code (Lexing.lexeme_char lexbuf (i+2)) - 48))

 (* Keywords *)

 let keywords = [
   ("%token", Parser.TOKEN); 
   ("%start", Parser.START); 
   ("%empty", Parser.EMPTY)
 ]

 let keyword_table = 
   (Hashtbl.create (List.length (keywords)) : (string, Parser.token) Hashtbl.t)

 let () =
   List.iter (fun (str, tok) -> Hashtbl.add (keyword_table) (str) (tok)) (keywords)

}

(* Auxiliary regular expressions *)

let newline = '\n' | '\r' | "\r\n"
let blank = ' ' | '\t'
let digit = ['0' - '9']
let letter = ['a' - 'z' 'A' - 'Z']
let alpha = digit | letter
let ident = letter (('_')* alpha)*
let decimal_literal = ['0'-'9']+
let float_literal = ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?


(* RULES *)

rule main = parse 
  newline    { Lexing.new_line lexbuf; main lexbuf }
| blank+     { main lexbuf }
| "(*" 
   { 
    comment_depth := 1;
    handle_lexical_error (comment) (lexbuf);
    main (lexbuf)
   }

| '|'    { Parser.MID }
| "::="  { Parser.ASSIGNMENT }
| "..."  { Parser.ELLIPSIS }
| "["    { Parser.LBRACKET }
| "]"    { Parser.RBRACKET } 
| "{"    { Parser.LBRACE } 
| "}"    { Parser.RBRACE } 
| "("    { Parser.LPAREN } 
|  ")"   { Parser.RPAREN } 
| "+"    { Parser.PLUS } 
| "*"    { Parser.TIMES } 
| ';'    { Parser.SEMICOLON }
| "%%"   { Parser.PCENTPCENT }

| '%' ident
   {
     let id = Lexing.lexeme (lexbuf)
     in try
          Hashtbl.find (keyword_table) (id)
        with Not_found -> 
               raise (Error (Illegal_keyword (String.escaped (id),
                                              Location.lexeme_loc (lexbuf))))

   }

| ident 
   {
    Parser.IDENT (Lexing.lexeme (lexbuf))
   }

| '"' 
   {
    reset_string_buffer ();
    handle_lexical_error (string) (lexbuf);
    Parser.STRING (get_stored_string ()) 
   }

| eof 
   {
    Parser.EOF
   }

| _
   { 
    raise (Error (Illegal_character (String.escaped (Lexing.lexeme (lexbuf)),
                                     Location.lexeme_loc (lexbuf))))

   }


and keyword = parse
  ident 
    {
     let id = Lexing.lexeme (lexbuf)
     in try
          Hashtbl.find (keyword_table) (id)
        with Not_found -> 
               raise (Error (Illegal_keyword (String.escaped (id),
                                              Location.lexeme_loc (lexbuf))))
    }

| _
    {
     raise (Error (Keyword_expected (Location.lexeme_loc (lexbuf))))
    }


and string = parse
  '"' 
    { 
     () 
    }

| '\\'   [' ' '\013' '\009' '\012']*   '\010'   [' ' '\013' '\009' '\012']*
    { 
     string (lexbuf)
    }

| '\\' ['\\' '"' 'n' 't' 'b' 'r'] 
    { 
     store_string_char (char_for_backslash (Lexing.lexeme_char (lexbuf) (1)));
     string (lexbuf)
    }

| '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] 
    { 
     store_string_char (char_for_decimal_code (lexbuf) (1));
     string (lexbuf)
    }

| eof 
    { 
     raise (Locally_handled_error (Local_unterminated_string)) 
    }

| '\010'
    { 
     store_string_char ('\010');
     string (lexbuf)
    }
  
| _ 
    { 
     store_string_char (Lexing.lexeme_char (lexbuf) (0));
     string (lexbuf)
    }


and comment = parse
  "(*" 
    {
     incr (comment_depth); 
     comment (lexbuf)
    }

| "*)" 
    {
     decr (comment_depth);
     if !comment_depth = 0 then () else comment (lexbuf)
    }

| '"' 
    { 
     reset_string_buffer();
     string (lexbuf);
     reset_string_buffer();
     comment (lexbuf) 
    }

| "''"
    { comment (lexbuf) }

| "'" [^ '\\' '\''] "'"
    { comment (lexbuf) }

| "'\\" ['\\' '\'' 'n' 't' 'b' 'r'] "'"
    { comment (lexbuf) }

| "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { comment (lexbuf) }

| eof 
    { raise (Locally_handled_error (Local_unterminated_comment)) }

| '\010'
    { comment (lexbuf) }
| _ 
    { comment (lexbuf) }


{

 (* Watson's lexer trailer *)

  let eof : Parser.token = Parser.EOF

(*-----------------*)
(* Pretty-printing *)
(*-----------------*)

let token_to_string = 
  let open Parser
in function
  TOKEN       -> "\n%token "
| START       -> "\n%start "
| EMPTY       -> "%empty"
| MID         -> "\n| "
| ASSIGNMENT  -> " ::=\n"
| ELLIPSIS    -> " ..."
| LBRACKET    -> " ["
| RBRACKET    -> "]"
| LBRACE      -> " {"
| RBRACE      -> "}"
| LPAREN      -> " ("
| RPAREN      -> ")"
| PLUS        -> "+"
| TIMES       -> "*"
| SEMICOLON   -> "\n;\n"
| IDENT (s)   -> " " ^ s
| STRING (s)  -> " \"" ^ s ^ "\""
| PCENTPCENT  -> "\n%%\n"
| EOF         -> "EOF"


let get_token ?(trace : out_channel option) (lexbuf) =
  let token = main (lexbuf)
in begin match trace with
     Some (channel) ->
       output_string (channel) (token_to_string (token));
       flush (channel)
   | None -> ()
   end;
   token
  
let standalone ~(src:string) : unit  =
  let in_hdl = new Standard.IO.in_handler (src) in 
  let lexbuf = Lexing.from_channel (in_hdl#channel) in
  let trace_name = in_hdl#filename ^ ".lexer_trace" in
  let out_hdl = new Standard.IO.out_handler (trace_name) in
  let rec aux (lex_buf) = (* ARGUMENT MANDATORY (side-effect on lexbuf enough) ??? *)
    match get_token ~trace:(out_hdl#channel) (lex_buf) with
      EOF -> begin
                in_hdl#close;
                out_hdl#close
             end
    | x   -> aux (lex_buf)
in aux (lexbuf)
  
}
