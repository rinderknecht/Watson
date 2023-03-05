%{

(* 
   The global reference [gram_tokens] handles the tokens defined
   \emph{inside} the grammar (used when parsing the grammar keywords) 
*)


module OrderedTokens =
  struct
    type t = string
    let compare = Pervasives.compare
  end

module Tokens = Set.Make (OrderedTokens)

let gram_tokens = ref (Tokens.empty)

let syntax_error (pos) (msg) =
  raise (Syntax.Error (Syntax.Other (msg, Location.rhs_loc (pos))))

let unclosed (opening_name) (opening_num) (closing_name) (closing_num) =
  raise (Syntax.Error (Syntax.Unclosed (Location.rhs_loc (opening_num), opening_name,
                          Location.rhs_loc (closing_num), closing_name)))

%}

%token TOKEN
%token START
%token EMPTY
%token MID
%token ASSIGNMENT
%token ELLIPSIS
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token PLUS
%token TIMES
%token SEMICOLON
%token <string> IDENT
%token <string> STRING
%token PCENTPCENT
%token EOF

%start main
%type <filename:string -> Core.AST.t> main 

%%

main:
  token_dcl start PCENTPCENT grammar EOF
    { 
     new Core.AST.t ($1) ($4) ($2)
    } 
;


token_list:
  IDENT token_list  
   { 
    let token_list = new Core.AST.token ($1) (Location.rhs_loc (1)) :: $2 
    in begin
         gram_tokens := Tokens.add ($1) (!gram_tokens);
         token_list
       end
   }
| /* empty */
   { 
    [] 
   }
;

token_dcl: 
  TOKEN IDENT token_list token_dcl  
    { 
     let token_list = new Core.AST.token ($2) (Location.rhs_loc (2)) :: $3 @ $4 
     in begin
          gram_tokens := Tokens.add ($2) (!gram_tokens);
          token_list
        end
    }
| /* empty */ 
    { 
     [] 
    }
;

start:
  START IDENT  { new Core.AST.non_terminal ($2) (Location.rhs_loc (2)) }
| START error  
   { 
    syntax_error (2) ("Entry point expected (ie. main rule name).")
   }
;

grammar:
  rule other_rules  { $1 :: $2 }
;

other_rules:
  grammar      { $1 }
| /* empty */  { [] }
;

rule:
  IDENT ASSIGNMENT production SEMICOLON
    {
     Core.AST.Rule (new Core.AST.non_terminal ($1) (Location.rhs_loc (1)), $3)
    } 
;

production:
  rhs other_rhs  
    { 
     new Core.AST.production ($1::$2) (Location.symbol_loc ())
    }
;

other_rhs:
  /* empty */        { [] }
| MID rhs other_rhs  { $2::$3 }
;

rhs:
  rhs_item other_rhs_items  
    { Core.AST.Items (new Core.AST.non_empty_rhs ($1::$2) (Location.symbol_loc ())) }
| EMPTY                     
    { Core.AST.Empty (Location.rhs_loc (1)) }
;

other_rhs_items:
  /* empty */               { [] }
| rhs_item other_rhs_items  { $1::$2 }
;

rhs_item:
  word      { Core.AST.Basic ($1)    }
| extended  { Core.AST.Extended ($1) }
;


word:
  IDENT  
    { 
     if   Tokens.mem ($1) (!gram_tokens)
     then Core.AST.Terminal (new Core.AST.token ($1) (Location.rhs_loc (1)))
     else Core.AST.Non_Terminal (new Core.AST.non_terminal ($1) (Location.rhs_loc (1)))
    }
| STRING
    {
     Core.AST.Terminal (new Core.AST.concrete ($1) (Location.rhs_loc (1)))
    } 
;

extended: 
  LBRACKET production RBRACKET  
    { 
     Core.AST.Option (new Core.AST.wrapped_production ($2) (Location.symbol_loc ()))
    }
| LBRACKET production error
    {
     unclosed ("[") (1) ("]") (3)
    } 
| LPAREN production RPAREN PLUS  
    { 
     Core.AST.Plus (new Core.AST.wrapped_production ($2) (Location.symbol_loc ()))
    }
| LPAREN production RPAREN
    {
     Core.AST.Brackets (new Core.AST.wrapped_production ($2) (Location.symbol_loc ()))
    } 
| LPAREN production error
    {
     unclosed ("(") (1) (")") (3)
    } 
| word PLUS
    {
     let rhs = 
       Core.AST.Items (new Core.AST.non_empty_rhs [Core.AST.Basic ($1)] 
                                                  (Location.rhs_loc (1))) in
       let production = 
         new Core.AST.production [rhs] (Location.rhs_loc (1))
     in Core.AST.Plus (new Core.AST.wrapped_production (production) 
                                                       (Location.symbol_loc ()))
    } 
| LPAREN production RPAREN TIMES 
    { 
     Core.AST.Star (new Core.AST.wrapped_production ($2) (Location.symbol_loc ()))
    }
| word TIMES
    {
     let rhs = 
       Core.AST.Items (new Core.AST.non_empty_rhs [Core.AST.Basic ($1)] 
                                                  (Location.rhs_loc (1))) in
       let production = new Core.AST.production [rhs] (Location.rhs_loc (1))
     in Core.AST.Star (new Core.AST.wrapped_production (production) 
                                                       (Location.symbol_loc ()))
    } 
| LBRACE word word ELLIPSIS RBRACE PLUS
    {
     let repeated_term =
       match $2 with
         Core.AST.Terminal _ ->
           syntax_error (2) ("Non-terminal expected as repeated term.")
       | Core.AST.Non_Terminal (non_term) ->
           non_term in
       let separator = $3
     in Core.AST.List_Plus (new Core.AST.repetition (repeated_term) 
                                                    (separator) 
                                                    (Location.symbol_loc ()))
    } 
| LBRACE word word ELLIPSIS error
    {
     unclosed ("{") (1) ("}") (3)
    } 
| LBRACE word word ELLIPSIS RBRACE TIMES
    {
     let repeated_term =
       match $2 with
         Core.AST.Terminal _ ->
           syntax_error (2) ("Non-terminal expected as repeated term.")
       | Core.AST.Non_Terminal (non_term) ->
           non_term in
       let separator = $3
     in Core.AST.List_Star (new Core.AST.repetition (repeated_term) 
                                                    (separator) 
                                                    (Location.symbol_loc ()))
    } 
;
