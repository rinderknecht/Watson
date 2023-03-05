(*=============*)
(* Main module *)
(*=============*)

module Command_line_option =
  struct
(*----------------------*)
(* Command-line parsing *)
(*----------------------*)

let additional_pad = "   "

let make_pad (s) = String.make (String.length (s) + String.length (additional_pad)) (' ')


(*-------------------*)
(* The -debug option *)
(*-------------------*)

type debug_mode =
  Lexing
| Parsing
| ParsingOnly
| Abstract
| AbstractOnly

let debug = ref (None : debug_mode option)

let debug_hdl = function
  "lexing"        -> debug := Some (Lexing)
| "parsing"       -> debug := Some (Parsing)
| "parsing-only"  -> debug := Some (ParsingOnly)
| "abstract"      -> debug := Some (Abstract)
| "abstract-only" -> debug := Some (AbstractOnly)
| _ -> begin
         prerr_endline (Sys.argv.(0) ^ ": Please give a valid debugging mode (lexing or parsing or parsing-only or abstract or abstract-only).");
         exit (-1)
       end
 
let debug_option_name = "-debug"

let debug_padding = make_pad (debug_option_name)

let debug_doc =
  "<mode>"
^ "  Debugging mode. <mode> can be:\n"
^ debug_padding
^ "          lexing         Lexing is traced.\n"
^ debug_padding
^ "          parsing        Lexing and parsing are parsed.\n"
^ debug_padding
^ "          parsing-only   Only the parsing is traced.\n"
^ debug_padding
^ "          abstract       Lexing and parsing are parsed\n"
^ debug_padding
^ "                         and a pretty-print from\n"
^ debug_padding
^ "                         the abstract data type is done.\n"
^ debug_padding
^ "          abstract-only  Only a pretty-print from the\n"
^ debug_padding
^ "                         abstract data type is done."


(*--------------------------*)
(* The -caml-streams option *)
(*--------------------------*)

let caml_streams = ref (false)

let caml_streams_option_name = "-caml-streams"

let caml_streams_padding = make_pad (caml_streams_option_name)

let caml_streams_doc = 
  " Enforce LL(1) restriction in order to comply with\n"
^ caml_streams_padding
^ " Objective Caml streams semantics."


(*--------------------*)
(* The -reduce option *)
(*--------------------*)

let reduce = ref (None : string option)

let reduce_option_name = "-reduce"

let reduce_padding = make_pad (reduce_option_name)

let reduce_hdl (filename) =
  reduce := Some (filename)

let reduce_doc = 
  "<file> Get rid of extended operators and output in <file>\n"
^ reduce_padding
^ "       a strict and equivalent BNF grammar."


(*---------------------------*)
(* Anonymous argument action *)
(*---------------------------*)

let grammar_filename = ref (None : string option)

let anonymous_arg_action (filename) = 
  grammar_filename := Some (filename)


(*---------------------------------------*)
(* Specification of command-line options *)
(*---------------------------------------*)

let speclist = [
  (caml_streams_option_name, Arg.Set (caml_streams), caml_streams_doc);
  (reduce_option_name, Arg.String (reduce_hdl), reduce_doc);
  (debug_option_name, Arg.String (debug_hdl), debug_doc)
] 

(*---------------*)
(* Usage message *)
(*---------------*)

let usage_msg (soft_name) = 
  "Usage: " ^ soft_name ^ " <file> <option>*\nOptions are:"


let parse () =
  Arg.parse (speclist) (anonymous_arg_action) (usage_msg (Sys.argv.(0)))

end



module Report =
  struct
let lexical_error_header = "Watson reports the following lexical error:"

let lexical_error (filename) (error) =
  begin
    Format.print_string (lexical_error_header);
    Format.force_newline ();
    match error with
      Lexer.Illegal_keyword (ident, loc) ->
        Format.printf ("Illegal keyword \"%s\".") (ident);
        Format.force_newline ();
        Location.print (loc) (filename)
    | Lexer.Illegal_character (ident, loc) ->
        Format.printf ("Illegal character \"%s\".") (ident);
        Format.force_newline ();
        Location.print (loc) (filename)
    | Lexer.Keyword_expected (loc) ->
        Format.print_string ("Keyword expected.");
        Format.force_newline ();
        Location.print (loc) (filename)
    | Lexer.Unterminated_string (loc) ->
        Format.print_string ("Unterminated string.");
        Format.force_newline ();
        Location.print (loc) (filename)
    | Lexer.Unterminated_comment (loc) ->
        Format.print_string ("Unterminated comment.");
        Format.force_newline ();
        Location.print (loc) (filename)
  end
  
let syntactic_error_header = "Watson reports the following syntax error:"

let syntax_error (filename) (error) = 
  begin
    Format.print_string (syntactic_error_header);
    Format.force_newline ();
    match error with
      Syntax.Unclosed (opening_loc, opening, closing_loc, closing) ->
        Format.printf ("Symbol '%s' expected.") (closing);
        Format.force_newline ();
        Location.print (closing_loc) (filename);
        Format.print_string ("Watson reports also:");
        Format.force_newline ();
        Format.printf ("This '%s' might be unmatched.") (opening);
        Format.force_newline ();
        Location.print (opening_loc) (filename)

    | Syntax.Other (msg, loc) ->
        Format.print_string (msg); 
        Format.force_newline ();
        Location.print (loc) (filename)
  end


let semantic_error_header = "Watson reports the following semantic error:"


let core_error (filename) (error) = 
  begin
    Format.print_string (semantic_error_header);
    Format.force_newline ();
    match error with
      Core.Grammar.Rule_double_decl (rule_name, fst_occ_loc, snd_occ_loc) ->
        Format.printf ("Rule \"%s\" is declared twice.") (rule_name);
        Format.force_newline ();
        Location.print (fst_occ_loc) (filename);
        Location.print (snd_occ_loc) (filename)

    | Core.Grammar.Missing_axiom (non_terminal) ->
        Format.printf ("Axiom \"%s\" is missing.") (non_terminal#name);
        Format.force_newline ();
        Location.print (non_terminal#loc) (filename)

    | Core.Grammar.Missing_rule (non_terminal) ->
        Format.printf ("Rule \"%s\" must be defined.") (non_terminal#name);
        Format.force_newline ();
        Location.print (non_terminal#loc) (filename)
  end

let warning_header = "Watson reports the following semantic warning:"


let core_warning (filename) = function
  Core.Grammar.Token_double_decl (token_name, fst_occ_loc, snd_occ_loc) ->
    begin
      Format.print_string (warning_header);
      Format.force_newline ();
      Format.printf ("Token \"%s\" is declared twice.") (token_name);
      Format.force_newline ();
      Location.print (fst_occ_loc) (filename);
      Location.print (snd_occ_loc) (filename)
    end
| Core.Grammar.Useless_rules (lhs_occs) ->
    let sorting_pred (rule_name0, lhs_loc0) (rule_name1, lhs_loc1) =
      lhs_loc0 <= lhs_loc1 in
    let sorted_lhs_occs = Sort.list (sorting_pred) (lhs_occs) in
    let (sorted_rule_names, sorted_lhs_locs) = List.split (sorted_lhs_occs) in
      let formatted_rule_names = 
        String.concat (", ") (List.map (fun x -> "\"" ^ x ^ "\"") (sorted_rule_names))
    in begin
         Format.print_string (warning_header);
         Format.force_newline ();
         (if   List.length (sorted_lhs_locs) = 1 
          then Format.printf ("Rule \"%s\" is useless.") (formatted_rule_names)
          else Format.printf ("Rules %s are useless.") (formatted_rule_names)
         );
         Format.force_newline ();
         List.iter (fun loc -> Location.print (loc) (filename)) (sorted_lhs_locs)
       end

let core_warnings (filename) = List.iter (core_warning (filename))


(*i
let base_error (filename) (error) =
  begin
    Format.print_string (semantic_error_header);
    Format.force_newline ();
    match error with
      Base.Grammar.Empty_derivation_in_extended_operator (loc) ->
        begin
          Format.printf ("Operand must not derive the empty word.");
          Format.force_newline ();
          Location.print (loc) (filename)
        end
  end
*)

(*
let strm_error (filename) (error) =
  begin
    Format.print_string (semantic_error_header);
    Format.force_newline ();
    match error with
      Strm_grm.Empty_word_in_head (rhs_elm_name, loc) ->
        begin
          Format.printf ("Word '%s' derives empty at the head of production.") (rhs_elm_name);
          Format.force_newline ();
          Location.print (loc) (filename)
        end
    | Strm_grm.Badly_placed_rhs (loc) ->
        begin
          Format.printf ("Productions deriving the empty word must be the last in the rule.");
          Format.force_newline ();
          Location.print (loc) (filename)
        end
    | Strm_grm.Left_recursion (head, nt_occs) ->
        let (name_path, loc_path) = List.split (List.rev (head :: nt_occs)) in
          let formatted_path = 
            String.concat (", ") (List.map (fun x -> "\"" ^ x ^ "\"") (name_path))
        in begin
             Format.printf ("Rule \"%s\" is left-recursive: %s.") (rule_name) (formatted_path);
             Format.force_newline ();
             List.iter (fun loc -> Location.print (loc) (filename)) (loc_path)
           end
    end
  end
i*)


let internal_failure (filename) (message) =
  begin
    prerr_endline ("Watson reports the following internal failure:");
    prerr_endline (message);
    prerr_endline ("Please report.");
  end


let lexing_fatal_error (filename) (message) =
  begin
    prerr_endline ("Watson reports the following fatal error during lexing:");
    prerr_endline (message);
    prerr_endline ("Please report.");
  end



  end

(*-------------------------*)
(* Unwrapped main function *)
(*-------------------------*)

let unwrapped_main (filename) = 
  let lex_dump_file = filename ^ ".lexer_trace" in
  let par_dump_file = filename ^ ".parser_trace" in
  let ast =
    match !Command_line_option.debug with
      None 
    | Some (Command_line_option.Abstract | Command_line_option.AbstractOnly) ->
        Parse.grammar (filename)
    | Some (Command_line_option.Lexing) -> 
        Parse.grammar ~lexing:(Parse.Tracing_to (lex_dump_file)) 
                      (filename)
    | Some (Command_line_option.Parsing) ->
        Parse.grammar ~parsing:(Parse.Tracing_to (par_dump_file))
                      ~lexing:(Parse.Tracing_to (lex_dump_file))
                      (filename)
    | Some (Command_line_option.ParsingOnly) ->
        Parse.grammar ~parsing:(Parse.Tracing_to (par_dump_file))
                      (filename) in 

  let grm = new Base.Grammar.c (ast) in

  let () = Report.core_warnings (filename) (grm#warnings) in

  let () = match !Command_line_option.debug with
             Some (  Command_line_option.Abstract 
                   | Command_line_option.AbstractOnly) ->

               (* Pretty-print *)
               grm#print (filename ^ ".pretty_print_from_adt");

               (* Print out the first tokens of the grammar. *)
               prerr_endline ("First terminals of each rules: ");
               Terminal.First.iter (
                 fun (rule_name : string) (terminals : Terminal.Set.t) ->
                   prerr_string (rule_name ^ " : ");
                   prerr_string (String.concat (", ") 
                                               (List.map (fun nt -> nt#ident) 
                                                         (Terminal.Set.elements (terminals))));
                   prerr_newline ()
               ) (grm#first_terminals);
               prerr_newline ();

(*i ONLY FOR Strm_grm.c !! 
               prerr_endline ("Empty word bit patterns:");
               Core.Grammar.RulenameMap.iter (
                 fun (rule_name : string) (bits) ->
                   let apply (acc) = function
                     true  -> acc ^ "1"
                   | false -> acc ^ "0" 
                   in begin 
                        prerr_string (rule_name ^ " : ");
                        prerr_string (Array.fold_left (apply) "" (bits));
                        prerr_newline ()
                      end
               ) (grm#epsilon_rhs);
               prerr_newline ();
i*)
               prerr_endline ("Rules deriving the empty word:");
               prerr_endline (String.concat (", ") (Base.Rule.Name.Set.elements (grm#epsilon_rules)));
               prerr_newline ()
           | _ -> ()
in (*i Only for Strm_grm.c !!!
   if   !caml_streams 
   then begin
          Grammar.empty_word_in_head (grm);  
          Grammar.badly_placed_production (grm)
        end
   else i*)
   (*i TO BE DONE
      match !Command_line_option.reduce with
          Some (_BNF_filename) -> 
            grm#reduce#print (_BNF_filename)
        | None -> ()
   i*)
   ()




(*------*)
(* MAIN *)
(*------*)

let run () =
  let () = Command_line_option.parse ()
in match !Command_line_option.grammar_filename with
     None -> begin
               prerr_endline (Sys.argv.(0) ^ ": E-BNF file missing.");
               exit (-1)
             end
   | Some (filename) ->
       try
         unwrapped_main (filename)
       with Lexer.Error (kind) ->
              Report.lexical_error (filename) (kind);
              exit (-1)
          | Syntax.Error (kind) ->
              Report.syntax_error (filename) (kind);
              exit (-1)
          | Core.Grammar.Error (kind) ->
              Report.core_error (filename) (kind);
              exit (-1)
(*i          | Base.Grammar.Error (kind) ->
              Report.base_error (filename) (kind);
              exit (-1)
i*)
          | Failure (msg) ->
              Report.internal_failure (filename) (msg);
              exit (-1)
          | Lexer.Fatal_error (msg) ->
              Report.lexing_fatal_error (filename) (msg);
              exit (-1)
              

let () = Printexc.catch run ()
