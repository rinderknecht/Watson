(* Wrapper for module Parser *)



type tracing_mode =
  Tracing_to of string
| No_tracing


let grammar ?(parsing = No_tracing) 
            ?(lexing = No_tracing)  
            (src : string) =

  let in_hdl = new Standard.IO.in_handler (src) in
  let lexbuf = Lexing.from_channel (in_hdl#channel) in

  let (lex_dump_hdl_opt, get_token) =
    match lexing with
      Tracing_to (dump_file) ->
        let out_hdl = new Standard.IO.out_handler ~filename:(dump_file)
        in (Some (out_hdl), Lexer.get_token ~trace:(out_hdl#channel))
    | No_tracing ->
        (None, Lexer.get_token ?trace:None)
in try
     let ast = Parser.main (get_token) (lexbuf) (src)
     in begin
          Parsing.clear_parser ();
          (match parsing with
             Tracing_to (par_dump_file) ->
               Core.AST.pretty_print (ast) (par_dump_file);
               Core.AST.debug_locations (ast) (src) (par_dump_file ^ ".dbg_loc") (* HTML ??? *)
           | No_tracing -> 
               ()
          );
          (match lex_dump_hdl_opt with
             Some (out_hdl) -> out_hdl#close
          |  None -> ()
          );
          ast
        end
   with Parsing.Parse_error -> 
          let loc = Location.lexeme_loc (lexbuf)
          in raise (Syntax.Error (Syntax.Other ("Syntax error.", loc)))
          


