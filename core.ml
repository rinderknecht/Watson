
(* Abstract Syntax Tree (AST) *)

module type AST_Sig =
  sig
    type rule_name = string

    class non_terminal :
      rule_name -> Location.t ->
      object
        method name : rule_name
        method loc  : Location.t
      end

    class terminal :
      string -> Location.t ->
      object
        method ident : rule_name
        method loc   : Location.t
      end
  
    type word =
      Terminal     of terminal
    | Non_Terminal of non_terminal

    class token :
      string -> Location.t ->
      object
        method ident : rule_name
        method loc   : Location.t
      end

    class concrete :
      string -> Location.t ->
      object
        method ident : rule_name
        method loc   : Location.t
      end

    class ['item] non_empty_rhs :
      'item list -> Location.t ->
      object
        method items : 'item list
        method loc   : Location.t
      end

    class ['rhs] production :
      'rhs list -> Location.t ->
      object
        method list_of_rhs : 'rhs list
        method loc         : Location.t
      end

    class ['rhs] wrapped_production :
      'rhs production -> Location.t ->
      object
        method emb_prod : 'rhs production
        method loc      : Location.t
      end

    class repetition :
      non_terminal -> word -> Location.t ->
      object
        method term      : non_terminal
        method separator : word
        method loc       : Location.t
      end

    type extended =
      Star      of (rhs) wrapped_production
    | Plus      of (rhs) wrapped_production
    | List_Plus of repetition
    | List_Star of repetition
    | Option    of (rhs) wrapped_production
    | Brackets  of (rhs) wrapped_production

    and item = 
      Basic    of word
    | Extended of extended

    and rhs = 
      Items of (item) non_empty_rhs
    | Empty of Location.t      

    type rule = Rule of non_terminal * (rhs) production

    class t : token list -> rule list -> non_terminal -> filename:string ->
      object 
        method filename : string
        method tokens : token list
        method rules : rule list
        method axiom : non_terminal
      end

    val string_of_word       : word -> string
    val string_of_extended   : extended -> string
    val string_of_item       : item -> string
    val string_of_rhs        : rhs -> string
    val string_of_production : rhs production -> string
    val string_of_rule       : rule -> string

    val print_tokens : out_channel -> token list -> unit
    val print_axiom  : out_channel -> non_terminal -> unit
    val print_rules  : out_channel -> rule list -> unit

    val pretty_print : t -> filename:string -> unit

    val debug_locations : t -> src:string -> dst:string -> unit

    val loc_of_word         : word -> Location.t
    val loc_of_extended     : extended -> Location.t
    val loc_of_item         : item -> Location.t
  end



module AST =
  struct
    type rule_name = string

    class non_terminal (r_name : rule_name) (location : Location.t) =
      object
        method name = r_name
        method loc  = location
      end

    class terminal (id : string) (location : Location.t) =
      object
        method ident = id
        method loc   = location
      end

    type word =
      Terminal     of terminal
    | Non_Terminal of non_terminal

    class token (id : string) (location : Location.t) =
      object
        inherit terminal (id) (location)
      end

    class concrete (id : string) (location : Location.t) =
      object
        inherit terminal (id) (location) as super

        method ident = "\"" ^ super#ident ^ "\""
      end

    class ['item] non_empty_rhs (i : 'item list) (location : Location.t) =
      object
        method items = i
        method loc = location
      end

    class ['rhs] production (rhs_lst : 'rhs list) (location : Location.t) =
      object
        method list_of_rhs = rhs_lst
        method loc = location
      end

    class ['rhs] wrapped_production (prod : 'rhs production) (location : Location.t) =
      object
        method emb_prod = prod
        method loc = location
      end

    class repetition (n_terminal : non_terminal) (sep : word) (location : Location.t) =
      object
        method term = n_terminal
        method separator = sep
        method loc = location
      end

    type extended =
      Star      of (rhs) wrapped_production
    | Plus      of (rhs) wrapped_production
    | List_Plus of repetition
    | List_Star of repetition
    | Option    of (rhs) wrapped_production
    | Brackets  of (rhs) wrapped_production

    and item = 
      Basic    of word
    | Extended of extended

    and rhs = 
      Items of (item) non_empty_rhs
    | Empty of Location.t      

    type rule = Rule of non_terminal * (rhs) production

    (* Grammar type. See interface for documentation. *)

    class t (token_list : token list) (rule_list : rule list) 
            (entry_point : non_terminal) ~filename:(filename : string)=
      object 
        method filename = filename
        method tokens = token_list
        method rules = rule_list
        method axiom = entry_point
      end


    (* Pretty-printing *)

    let string_of_word = function
      Terminal (terminal)         -> terminal#ident
    | Non_Terminal (non_terminal) -> non_terminal#name

    let rec string_of_extended = function
      Star (wrapped_production) ->
        let str_repr = string_of_production (wrapped_production#emb_prod)
        in (match wrapped_production#emb_prod#list_of_rhs with
              [Items (non_empty_rhs)] when List.length (non_empty_rhs#items) = 1 
                -> str_repr ^ "*"
            | _ -> "(" ^ str_repr ^ ")*"
           )

    | Plus (wrapped_production) -> 
        let str_repr = string_of_production (wrapped_production#emb_prod)
        in (match wrapped_production#emb_prod#list_of_rhs with
              [Items (non_empty_rhs)] when List.length (non_empty_rhs#items) = 1 
                -> str_repr ^ "+"
            | _ -> "(" ^ str_repr ^ ")+"
           )

    | List_Plus (repetition) -> 
        "{" ^ repetition#term#name ^ " " ^ string_of_word (repetition#separator) ^ "...}+"

    | List_Star (repetition) ->
        "{" ^ repetition#term#name ^ " " ^ string_of_word (repetition#separator) ^ "...}*"

    | Option (wrapped_production) -> 
        "[" ^ string_of_production (wrapped_production#emb_prod) ^ "]"

    | Brackets (wrapped_production) ->
        "(" ^ string_of_production (wrapped_production#emb_prod) ^ ")"

    and string_of_item = function
      Basic (word)        -> string_of_word (word)
    | Extended (extended) -> string_of_extended (extended)

    and string_of_production (prod) =
      String.concat (" | ") (List.map (string_of_rhs) (prod#list_of_rhs))

    and string_of_rhs = function
      Items (rhs) -> 
        String.concat (" ") (List.map (string_of_item) (rhs#items))
    | Empty (loc) ->
       "%empty"

    let string_of_rule (Rule (non_terminal, production)) =
      non_terminal#name ^ " ::=\n" ^ string_of_production (production)

    let print_tokens (out_chan) =
      List.iter (fun token -> output_string (out_chan) ("%token " ^ token#ident ^ "\n"))

    let print_axiom (out_chan) (non_terminal) =
      output_string (out_chan) ("%start " ^ non_terminal#name ^ "\n")

    let print_rules (out_chan) =
      List.iter (fun r -> output_string (out_chan) (string_of_rule (r) ^ "\n;\n\n"))

    let pretty_print (grammar) ~filename =
      let out_hdl = new Standard.IO.out_handler (filename)
    in begin 
         print_tokens (out_hdl#channel) (grammar#tokens);
         print_axiom (out_hdl#channel) (grammar#axiom);
         output_string (out_hdl#channel) ("\n%%\n\n");
         print_rules (out_hdl#channel) (grammar#rules);
         out_hdl#close;
       end


    (* For location debugging *)

    let debug_non_terminal (in_filename) (non_terminal) =
      non_terminal#name ^ Location.to_string (non_terminal#loc) (in_filename)

    let debug_terminal (in_filename) (terminal) = 
      terminal#ident ^ Location.to_string (terminal#loc) (in_filename)

    let debug_word (in_filename) = function
      Terminal (terminal)         -> debug_terminal (in_filename) (terminal)
    | Non_Terminal (non_terminal) -> debug_non_terminal (in_filename) (non_terminal)

    let rec debug_extended (in_filename) = function
      Star (wrapped_production) -> 
        "(" ^ debug_production (in_filename) (wrapped_production#emb_prod) ^ ")*"
      ^ Location.to_string (wrapped_production#loc) (in_filename)
    | Plus (wrapped_production) -> 
        "(" ^ debug_production (in_filename) (wrapped_production#emb_prod) ^ ")+" 
      ^ Location.to_string (wrapped_production#loc) (in_filename)
    | List_Plus (repetition) ->
        "{" ^ debug_non_terminal (in_filename) (repetition#term) 
      ^ " " ^ debug_word (in_filename) (repetition#separator) ^ "...}+"
      ^ Location.to_string (repetition#loc) (in_filename)
    | List_Star (repetition) ->
        "{" ^ debug_non_terminal (in_filename) (repetition#term) 
      ^ " " ^ debug_word (in_filename) (repetition#separator) ^ "...}*"
      ^ Location.to_string (repetition#loc) (in_filename)
    | Option (wrapped_production) ->
        "[" ^ debug_production (in_filename) (wrapped_production#emb_prod) ^ "]" 
      ^ Location.to_string (wrapped_production#loc) (in_filename)
    | Brackets (wrapped_production) ->
        "(" ^ debug_production (in_filename) (wrapped_production#emb_prod) ^ ")" 
      ^ Location.to_string (wrapped_production#loc) (in_filename)

    and debug_item (in_filename) = function
      Basic (word)        -> debug_word (in_filename) (word)
    | Extended (extended) -> debug_extended (in_filename) (extended)

    and debug_production (in_filename) (prod) =
      String.concat (" | ") (List.map (debug_rhs (in_filename)) (prod#list_of_rhs))

    and debug_rhs (in_filename) = function
      Items (rhs) ->
        String.concat (" ") (List.map (debug_item (in_filename)) (rhs#items))
    | Empty (loc) -> "%empty" ^ Location.to_string (loc) (in_filename)

    let debug_rule (in_filename) (Rule (non_terminal, production)) =
      non_terminal#name ^ Location.to_string (non_terminal#loc) (in_filename) 
    ^ " ::=\n" ^ debug_production (in_filename) (production)

    let debug_tokens (in_filename) (out_chan) =
      List.iter (fun (token) -> output_string (out_chan) ("%token" ^ Location.to_string (token#loc) (in_filename) 
                                                      ^ " " ^ token#ident ^ "\n"))

    let debug_axiom (in_filename) (out_chan) (non_terminal) =
      output_string (out_chan) ("%start " ^ non_terminal#name ^ "\n")

    let debug_rules (in_filename) (out_chan) =
      List.iter (fun r -> output_string (out_chan) (debug_rule (in_filename) (r) ^ "\n;\n\n"))

    let debug_locations (grammar) ~src ~dst =
      let out_hdl = new Standard.IO.out_handler (dst)
    in begin 
         debug_tokens (src) (out_hdl#channel) (grammar#tokens);
         debug_axiom (src) (out_hdl#channel) (grammar#axiom);
         output_string (out_hdl#channel) ("\n%%\n\n");
         debug_rules (src) (out_hdl#channel) (grammar#rules);
         out_hdl#close;
       end


    (* Location filters *)

    let loc_of_word = function
      Terminal (term)   -> term#loc
    | Non_Terminal (nt) -> nt#loc

    let loc_of_extended = function
      Star (wrapped_production)     -> wrapped_production#loc
    | Plus (wrapped_production)     -> wrapped_production#loc
    | List_Plus (repetition)        -> repetition#loc
    | List_Star (repetition)        -> repetition#loc
    | Option (wrapped_production)   -> wrapped_production#loc
    | Brackets (wrapped_production) -> wrapped_production#loc

    let loc_of_item = function
      Basic  (x)   -> loc_of_word (x)
    | Extended (x) -> loc_of_extended (x)

end





module Rule = Sem_rule.Make (struct type production = (AST.rhs) AST.production end)




(*=========================*)
(* Core-grammar definition *)
(*=========================*)

module Grammar =
  struct

(*--------*)
(* Errors *)
(*--------*)

type error =
  Rule_double_decl   of AST.rule_name * Location.t * Location.t
| Missing_axiom      of AST.non_terminal
| Missing_rule       of AST.non_terminal

exception Error of error


(*----------*)
(* Warnings *)
(*----------*)

type warning =
  Token_double_decl of string * Location.t * Location.t
| Useless_rules     of (string * Location.t) list




(*--------------------------------------------------------------------------*)
(* Core grammars. They are the simpliest semantic version of [AST.t]        *)

class c (grm_AST : AST.t) =
  let warnings = ref ([] : warning list) in

  (*------------------------------------------------------------------------*)
  (* [semantic_rules] is a map [Rule.Map_from_names.t] from rule names to [rule] objects. *)
  (* The invariant checked here is that rules are declared exactly once.    *)

  let (semantic_rules : Rule.Map_from_names.t) = 
    let add (m) (AST.Rule (nt, prod)) =
      try
        let prev_binding = Rule.Map_from_names.find (nt#name) (m)
        in raise (Error (Rule_double_decl 
                                   (nt#name, prev_binding#name_loc, nt#loc)))
      with Not_found -> 
             let grm_rule = new Rule.c (prod) (nt#loc)
             in Rule.Map_from_names.add (nt#name) (grm_rule) (m)
  in List.fold_left (add) (Rule.Map_from_names.empty) (grm_AST#rules)

  (*------------------------------------------------------------------------*)
in object (self)

    method warnings = !warnings

    (*-------------------------------------------*)
    (* Filename. For error reporting facilities. *)

    method filename = grm_AST#filename
   
    (*-------------------------------------------*)

    (*------------------------------*)
    (* Tokens. For pretty-printing. *)

    method tokens = grm_AST#tokens

    (*------------------------------*)


    (*-------------------------------------------------*)
    (* Rules. A map from rule names to [rule] objects. *)

    method rules = semantic_rules

    (*-------------------------------------------------*)


    (*--------------------------------------------*)
    (* Axiom. A non-terminal (includes location). *)

    method axiom = grm_AST#axiom

    (*--------------------------------------------*)


    (*--------------------------------------------------------------------------------*)
    (* Pretty-print to a file which name is [out_filename].                           *)

    method print ~filename =
      let out_hdl = new Standard.IO.out_handler (filename) in
      let rules = 
        let apply (rulename) (rule_contents) (acc) =       
          let nt = new AST.non_terminal (rulename) (rule_contents#name_loc)
        in (AST.Rule (nt, rule_contents#get_rhs))::acc
      in Rule.Map_from_names.fold (apply) (self#rules) []
    in begin
         AST.print_tokens (out_hdl#channel) (self#tokens);
         AST.print_axiom (out_hdl#channel) (self#axiom);
         output_string (out_hdl#channel) ("\n%%\n\n");
         AST.print_rules (out_hdl#channel) (rules);
         out_hdl#close
       end

    (*--------------------------------------------------------------------------------*)


    (*-------------------------------------------------*)
    (* Returns a shallow copy of the current instance. *)
    
    method copy = {< >}

    (*-------------------------------------------------*)


    (*--------------------------------------------------------------------------------*)
    (* This invariant assures that tokens are declared exactly once.                  *)
    (* If it is not the case, a _warning_ is issued:                                  *)
    (* [Token_double_decl (token_name, prev_loc, loc)]                                *)
    (* where [token_name] is the string characterizing the token declared at least    *)  
    (* twice, [prev_loc] is the location of the first met occurrence, and [loc] is    *)      
    (* the location of the second occurrence.                                         *)

    initializer   
      let module TokenIdMap = Standard.String.Map in
        let add (token_map) (token) =
          begin
            (try
               let prev_loc = TokenIdMap.find (token#ident) (token_map)
               in warnings := (Token_double_decl (token#ident, prev_loc, token#loc))::!warnings
             with Not_found -> ()
            );
            TokenIdMap.add (token#ident) (token#loc) (token_map)
          end in 
        let _ = List.fold_left (add) (TokenIdMap.empty) (grm_AST#tokens)
      in ()

    (*--------------------------------------------------------------------------------*)


    (*----------------------------------------------------------------------*)
    (* This invariant assures that the entry point is declared.             *)
    (* If it is not the case, an _error_ is issued by means of the raising  *)
    (* of exception [Error (Missing_axiom (axiom))] where *)
    (* [axiom] is the non-terminal corresponding to the axiom.              *)

    initializer
      try 
        let _ = Rule.Map_from_names.find (grm_AST#axiom#name) (self#rules)
        in ()
      with Not_found -> 
        raise (Error (Missing_axiom (grm_AST#axiom)))

    (*----------------------------------------------------------------------*)

    
    (*-----------------------------------------------------------------------------------*)
    (* This invariant assures:                                                           *)
    (*   1) Assures that every non-terminal right-occurrence (ie. in a right-hand side)  *)
    (*      has a definition (ie. appears in a left-hand side). The corresponding        *)
    (*      raised exception is:                                                         *)
    (*      [Error (Missing_rule (nt))] where [nt] stands for          *)
    (*      the right-occurrence of the undefined non-terminal.                          *)
    (*   2) Assures that there is no useless rule in the grammar, ie. all the rules      *)
    (*      can be unfolded from the axiom, ie. can be reached in a deep-first traversal *)
    (*      from the entry point. Note that it is not an _error_: a warning is emitted   *)
    (*      instead: [Useless_rules (inaccessible_rules)] where                 *)
    (*      [inaccessible_rules] is the list of the inaccessible rules (of type [rule]). *)

    initializer 
      let module Consistency =
        struct

          (*-----------------------------------------------------------------------*)
          (* Class [access] inherits from [[AST.non_terminal] My_stack.c].         *)
          (* The extra fields are:                                                 *)
          (*   1) [rules] is a set of rule names, initially empty.                 *)
          (* The extra methods are:                                                *)
          (*   1) [clear_path] is a renaming of the super-class' [clear] method.   *)
          (*   2) [extend_with_rule (nt)] extends the field [rules] with the rule  *)
          (*      name of the non-terminal [nt], and pushes [nt] on the current    *)
          (*      stack representation (see super-class).                          *)
          (*   3) [mem (rn)] returns [true] if and only if he rule name [rn]       *)
          (*      belongs to the set in field [rules], otherwise [false].          *)

          class access =
            object 
              inherit [AST.non_terminal] Standard.Stack.core as super

              val rules = Rule.Name.Set.empty

              method clear_path = super#clear
  
              method extend_with_rule (nt) = 
                {< rules = Rule.Name.Set.add (nt#name) (rules) >} # push (nt)

              method mem (rule_name) =
                Rule.Name.Set.mem (rule_name) (rules)
            end
          (*-----------------------------------------------------------------------*)

          (*------------------------------------------------------------------------*)
          (* Mutually recursive functions checking the set of accessible rules from *)
          (* a given entry point. In case a requested rule definition is missing an *)
          (* error is triggered (exception [Error (Missing_rule   *)
          (* (nt))], where [nt] is the missing non-terminal left-occurrence.). Each *)
          (* new accessible rule encountered is stored in the continuation [cont],  *) 
          (* which is an object of class [access] (see above).                      *)

          let rec check_word (cont) = function
            AST.Non_Terminal (nt) -> check_rule (cont) (nt)
          | AST.Terminal (_)      -> cont

          and check_extended (cont) = function
            AST.Star (wrapped_production)     -> check_prod  (cont) (wrapped_production#emb_prod)
          | AST.Plus (wrapped_production)     -> check_prod  (cont) (wrapped_production#emb_prod)
          | AST.List_Plus (repetition)  -> let cont0 = check_rule (cont) (repetition#term)
                                           in check_word (cont0) (repetition#separator)
          | AST.List_Star (repetition)  -> let cont0 = check_rule (cont) (repetition#term)
                                           in check_word (cont0) (repetition#separator)
          | AST.Option (wrapped_production)   -> check_prod  (cont) (wrapped_production#emb_prod)
          | AST.Brackets (wrapped_production) -> check_prod (cont) (wrapped_production#emb_prod) 

          and check_item (cont) = function
            AST.Basic (word)        -> check_word (cont) (word)
          | AST.Extended (extended) -> check_extended (cont) (extended)

          and check_rhs (cont) = function
            AST.Items (rhs) ->
              List.fold_left (check_item) (cont) (rhs#items)
          | AST.Empty (loc) -> cont

          and check_prod (cont) (prod) =
            List.fold_left (check_rhs) (cont) (prod#list_of_rhs)

          and check_rule (cont) (nt) =
            if   cont#mem (nt#name)
            then cont
            else try
                   let rule = Rule.Map_from_names.find (nt#name) (self#rules) in
                     let cont0 = cont#extend_with_rule (nt)
                   in check_prod (cont0) (rule#get_rhs)
                 with Not_found ->
                        raise (Error (Missing_rule (nt)))
          (*------------------------------------------------------------------------*)

          (*-------------------------------------------------------------------*)
          (* Computing the set of all accessible rules from the gramamr axiom. *)

          let accessible_rules = check_rule (new access) (self#axiom)
          (*-------------------------------------------------------------------*)

          (*------------------------------------------------------------------------*)
          (* Checking consistency of all remaining rules (thus including            *)
          (* inaccessible ones). Note that we start with the [accessible_rules] set *)
          (* in the continuation [cont], that is to say these previously checked    *)
          (* rules will not be checked again here.                                  *)

          let _ =
            let apply (rule_name) (rule) (cont) =
              check_rule (cont) (new AST.non_terminal (rule_name) (rule#name_loc))
          in Rule.Map_from_names.fold (apply) (self#rules) (accessible_rules#clear_path)
          (*------------------------------------------------------------------------*)

          (*---------------------------------------------------------------------*)
          (* Computing the set of all inaccessible rules of the grammar.         *)
          (* Basically, this goal is achieved by checking, for each rule in the  *)
          (* grammar, whether it belongs to the set of previously computed       *)
          (* accessible rules [accessible_rules].                                *)
 
          let inaccessible_rules =
            let apply (rule_name) (rule) (acc) =
              if   accessible_rules#mem (rule_name)
              then acc
              else (rule_name, rule#name_loc) :: acc
          in Rule.Map_from_names.fold (apply) (self#rules) [] 
          (*---------------------------------------------------------------------*)
        end
      in match Consistency.inaccessible_rules with
           []     -> ()
         | rules  -> warnings := Useless_rules (rules) :: !warnings

  (*-----------------------------------------------------------------------------------*)

  end

end
