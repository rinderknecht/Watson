

(* Abstract Syntax Tree (AST) *)

module type AST_Sig =
  sig

(*
 A \emph{rule name} ([rule_name]) is an identifier appearing in the
 left-hand side of an assignment symbol (\texttt{::=}). 
*)

type rule_name = string

(* A \emph{non-terminal} is an identifier appearing in the right-hand
 side of an assignment symbol, and which is also a rule name.
 The defining regular expression is (see lexer specification):\\
 \begin{tabular}{l}
   \texttt{let} digit = ['0' - '9'] \\
   \texttt{let} letter = ['a' - 'z' 'A' - 'Z'] \\
   \texttt{let} alpha = digit | letter \\
   \texttt{let} non\_terminal = letter (('\_')* alpha)*
 \end{tabular}
*)

class non_terminal :
  rule_name -> Location.t ->
  object
    method name : rule_name
    method loc  : Location.t
  end



(*
 Class [token] denotes identifiers declared with the \%token keyword.
 Class [concrete] corresponds to excerpts of concrete syntax, specified between
 double quotes in the Extended-BNF (E-BNF) source file. Both class [token] and
 [concrete] are sub-classes of [terminal].
*)

class terminal :
  string -> Location.t ->
  object
    method ident : rule_name
    method loc   : Location.t
  end
  
(*
  A word is either a terminal or a non-terminal. 
*)

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




(*
 A right-hand side ([rhs]) is basically a list of items.
 Unfortunately Objective Caml does not syntactically allow to
 mix mutually recursive classes and algebraic types, despite they both 
 share the same namespace. That is why the
 class [non_empty_rhs] is parametric: each occurrence of it in the AST
 will be instanciated with the (algebraic) type [item] (see below
 definition).
*)

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



(*
 The Backus-Naur Form (BNF) for grammars is here extended with rational operators.
 [Star] denotes the Kleene operator ("star"): repetition of an inner right-hand side.
 [Plus] is the same as [Star], except that the repetition is non empty.
 [List_Plus] is a non empty repetition of the pattern made of a non-terminal followed
 by a terminal. It is useful for characterizing non empty lists of concrete
 declarations terminated by a special symbol (like in the C programming language.)
 [List_Star] is the same as [List_Plus], except that the repetition may be empty.
 [Option] applies to an inner right-hand side, and denotes it or the empty word
 (sometimes called \emph{epsilon}).                                                          
*)

type extended =
  Star      of (rhs) wrapped_production
| Plus      of (rhs) wrapped_production
| List_Plus of repetition
| List_Star of repetition
| Option    of (rhs) wrapped_production
| Brackets  of (rhs) wrapped_production


(*
  An item is either a word or an application of an extended operator. 
*)

and item = 
  Basic    of word
| Extended of extended



(*
 An outermost right-hand side ([rhs]) is either a rhs ([Items])
 or the empty word (constructor [Empty]; concrete syntax is keyword \%empty).
 This is the reason of distinguishing inner and outermost rhs: only outermost rhs
 can \emph{syntactically} derive the empty word (ie. contain only the \%empty keyword).
 This syntactical property is very useful for semantics checkings and computations
 (e.g. reduction from Extended-BNF to BNF assumes that operands of extended
 operators do not derive the empty word.) and does not limitate the expressiveness
 of the grammars.
*)

and rhs = 
  Items of (item) non_empty_rhs
| Empty of Location.t      


(*
 A grammar [rule] is a pair made of a non-terminal
 (ie. a rule name) and a [production].             
*)

type rule = Rule of non_terminal * (rhs) production



(* Grammar type *)

class t : token list -> rule list -> non_terminal -> filename:string ->
  object 
    (* 
      [filename] is the name of the file containing the E-BNF.
      Useful for error reporting.                         
    *)
    method filename : string
    (* 
      [tokens] denotes the ordered list of tokens, introduced
      in the concrete syntax by the keywords \%token.     
    *)
    method tokens : token list
    (* [rules] contains the ordered list of rules. *)
    method rules : rule list
    (* 
      [axiom] records the entry point of the grammar,
      introduced in the concrete syntax by the keyword \%start. 
    *)
    method axiom : non_terminal
  end


(*
 Pretty-printing
*)

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


(* A REVOIR
 Location debugging. \\

 Usage: [debug_locations grm in_filename out_filename]
 [grm] is the grammar abstract syntax tree.
 [in_filename] is the concrete grammar filename
 [out_filename] is the dump file.
*)

val debug_locations : t -> src:string -> dst:string -> unit

val loc_of_word         : word -> Location.t
val loc_of_extended     : extended -> Location.t
val loc_of_item         : item -> Location.t

end


module AST : AST_Sig

module Rule : Sem_rule.S with type production = (AST.rhs) AST.production


(* Core-grammar definition *)


module Grammar :
  sig

(*
  Class [c] is the semantic version of [AST.t]. \\
  It takes as value argument a syntactic grammar                             
  (ie. a value of type [AST.t] and returns an object which provides the      
  following methods:
   \begin{itemize}
     \item [axiom] returns the axiom name and location of the grammar;
     \item [filename] returns the name of the file containing the
           grammar (ie. concrete syntax);           
     \item [rules] returns the semantic rules ([Rules.t]), which are a
           version of [AST.rule] adapted suited for the semantic
           computations;    
     \item [tokens] returns the token ([AST.token]) list;           
     \item [print (filename)] performs a pretty-print of the current
           grammar into the file whose filename is [filename];
     \item [copy] returns a shallow copy of the current instance.
   \end{itemize}

  The class invariants are:                                                  
   \begin{itemize}
     \item Rule names are unique. In case of double declaration,
           exception [Error (Rule_double_decl (rule_name, first_occ,
           sec_occ))] is raised at creation time, where [rule_name] is
           the name of the offending rule, [first_occ] is the location
           (of type [Location.t]) of the first occurrence of
           [rule_name], and [sec_occ] is the second one;
     \item Tokens are unique. In case of double declaration, a warning is
           reported at creation-time, on standard output.                        
     \item A rule whose name is the axiom (ie. the entry point) must
           be defined. Otherwise the exception [Error (Missing_axiom
           (nt))] is raised at creation-time, where [nt] is the
           non-terminal corresponding to the axiom;
     \item All right-occurrence of non-terminal must be defined
           (ie. appear in a left-occurrence). Otherwise the following
           exception is raised at creation-time: [Error (Missing_rule
           (nt))], where [nt] stands for the right-occurrence of the
           undefined non-terminal;
     \item There is no useless rule in the grammar, ie. all the rules
           can be unfolded from the axiom, ie. can be reached in a
           deep-first traversal from the entry point. Note that it is
           not an \emph{error}: a warning is issued instead.
   \end{itemize}
*)

type error =
  Rule_double_decl of AST.rule_name * Location.t * Location.t
| Missing_axiom    of AST.non_terminal
| Missing_rule     of AST.non_terminal

exception Error of error

type warning =
  Token_double_decl of string * Location.t * Location.t
| Useless_rules     of (string * Location.t) list

class c :
  AST.t ->
  object ('self)
    method warnings : warning list
    method axiom    : AST.non_terminal
    method filename : string
    method rules    : Rule.Map_from_names.t
    method tokens   : AST.token list
    method print    : filename:string -> unit
    method copy     : 'self
  end

end
