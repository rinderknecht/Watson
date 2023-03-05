module AST : Core.AST_Sig


module AST_bis :
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
        method index : int option
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
  end


module Rule : Sem_rule.S with type production = (AST.rhs) AST.production


module Grammar :
  sig
    class c : 
     Core.AST.t ->
     object
       inherit Core.Grammar.c

       method first_terminals : Terminal.First.t

       (* The set of rules deriving the empty word. *)
       method epsilon_rules : Rule.Name.Set.t
     end
  end

  
