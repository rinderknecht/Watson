module AST = Core.AST


module AST_bis =
  struct
    type rule_name = string

    class non_terminal = Core.AST.non_terminal

    class terminal = Core.AST.terminal

    type word =
      Terminal     of terminal
    | Non_Terminal of non_terminal

    class token = Core.AST.token

    class concrete = Core.AST.concrete

    class ['item] non_empty_rhs (i : 'item list) (location : Location.t) =
      object
        inherit ['item] Core.AST.non_empty_rhs (i) (location)
        method index : int option = None
      end

    class ['rhs] production = ['rhs] Core.AST.production

    class ['rhs] wrapped_production = ['rhs] Core.AST.wrapped_production

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
  end



module Rule = 
  Sem_rule.Make (struct type production = (AST.rhs) AST.production end)


module Grammar =
  struct

(* Left-recursive derivations.           *)

module OrderedCycle = 
  struct
    type t = AST.non_terminal list
    let compare = Pervasives.compare    (* What about circular permutations? *)
  end

module CycleSet = Set.Make (OrderedCycle)



module Continuation =
  struct
    (*-----------------------------------------------------------------------*)
    (* The class [RHS_index.c] is an index for right-hand side (rhs), useful *)
    (* when traversing a grammar.                                            *) 
    (* It contains the following fields:                                     *) 
    (*   1) [rhs_idx] is a counter of right-hand sides (rhs). The first rhs  *)
    (*      of the current rule is counted 0. By default (no current rule    *)
    (*      implied) [None] is assumed.                                      *)
    (* and the following methods:                                            *)
    (*   1) [clear] clears the field [rhs_idx], meaning "No current          *)
    (*      right-hand side."                                                *)
    (*   2) [incr] increments field [rhs_idx].                               *)
    (*   3) [get] returns the current index. If not defined, it raises       *)
    (*      [Undefined].                                                     *)

    module RHS_index =
      struct
        exception Undefined
        exception Invalid_index

        class c =
          object
            val index = None

            method clear = {< index = None >}
 
            method incr = 
              match index with
                Some (n) -> {< index = Some (n + 1) >}
              | None     -> {< index = Some (0)     >}

            method get =
              match index with
                Some (n) -> n
              | None     -> raise (Undefined)

            method set (n) =
              if   n >= 0
              then {< index = Some (n) >}
              else raise (Invalid_index)
          end
      end

    (*----------------------------------------------------------------------*)


    (*------------------------------------------------------------------------------*)

    class first_terminals =
      object (self : 'self)
        val terminals = Terminal.Set.empty

        val map = Terminal.First.empty

        method terminals : Terminal.Set.t = terminals

        method contains_empty = 
          Terminal.Set.mem (Terminal.Ordered.empty) (terminals)

        method map : Terminal.First.t = map

        method extend_map_under (rule_name : AST.rule_name) =
          {< map = Terminal.First.add (rule_name) (terminals) (map) >}

        method set (terms : Terminal.Set.t) = 
          {< terminals = terms >}

        method add_empty =
          {< terminals = Terminal.Set.add (Terminal.Ordered.empty) (terminals) >}

        method remove_empty =
          {< terminals = Terminal.Set.remove (Terminal.Ordered.empty) (terminals) >}

        method add (terminal : Terminal.Ordered.t) =
          {< terminals = Terminal.Set.add (terminal) (terminals) >}

        method clear =
          {< terminals = Terminal.Set.empty >}

        method update_with (other_term : 'self) =
          {< 
            terminals = Terminal.Set.union (terminals) (other_term#terminals);  
            map       = other_term#map
          >}
      end
    (*------------------------------------------------------------------------------*)

    (*-----------------------------------------------------------------------------------*)

    class epsilon (sem_grm : #Core.Grammar.c) =
      let (no_epsilon_rhs : (bool array) Rule.Name.Map.t) =
        let apply (rule_name) (rule) (acc) =
          Rule.Name.Map.add (rule_name) (Array.make (List.length (rule#get_rhs#list_of_rhs)) (false)) (acc)
      in Core.Rule.Map_from_names.fold (apply) (sem_grm#rules) (Rule.Name.Map.empty)
    in object
         val rules = Rule.Name.Set.empty

         val rhs : (bool array) Rule.Name.Map.t = no_epsilon_rhs

         method rules : Rule.Name.Set.t = rules

         method rhs : (bool array) Rule.Name.Map.t = rhs

         method add_rule (rule_name : AST.rule_name) =
           {< rules = Rule.Name.Set.add (rule_name) (rules) >}

         method set_rhs (rule_name : AST.rule_name) (rhs_idx : int) = 
           try
             let current_array = Rule.Name.Map.find (rule_name) (rhs)
             in begin
                  current_array.(rhs_idx) <- true;
                  {< >}
                end
           with Invalid_argument _ ->
                  let error_message = 
                    "[Make.epsilon#set_rhs (rule_name) (rhs_idx)]: Out-of-bound access.\n"
                  ^ "Args: rule_name = " ^ rule_name ^ ", rhs_idx = " ^ string_of_int (rhs_idx) ^ "."
                  in failwith (error_message)
              | Not_found ->
                  let error_message =
                    "[Make.epsilon#set_rhs (rule_name) (rhs_idx)]: Rule not found.\n"
                  ^ "Args: rule_name = " ^ rule_name ^ ", rhs_idx = " ^ string_of_int (rhs_idx) ^ "."
                  in failwith (error_message)
       end
    (*-----------------------------------------------------------------------------------*)



    (*-----------------------------------------------------------------------------------*)

    module EqElt = 
      struct
        type t = AST.non_terminal
        let equal (nt0) (nt1) = 
          nt0#name = nt1#name
      end 

    module Path = Standard.Stack.Make (EqElt)

    class c (sem_grm : #Core.Grammar.c) =
      object (self : 'self)

        method rules = sem_grm#rules

        val path = new Path.c

        method extend_path_with (nt : AST.non_terminal) = {< path = path#push (nt) >}

        method backtrack_one_step = 
          let (top, new_path) = path#pop 
        in {< path = new_path >}

        method clear_path = {< path = path#clear >}

        method mem_of_path : AST.non_terminal -> bool = path#mem


        val rhs_index = new RHS_index.c

        method clear_rhs_idx = {< rhs_index = rhs_index#clear >}

        method incr_rhs_idx = {< rhs_index = rhs_index#incr >}

        method get_rhs_idx = rhs_index#get

        method set_rhs_idx (n : int) = {< rhs_index= rhs_index#set (n) >}


        val first_terminals = new first_terminals

        method get_first_terminals = first_terminals

        method ft_map : Terminal.First.t = first_terminals#map

        method set_terminals (terms : Terminal.Set.t) = 
          {< first_terminals = first_terminals#set (terms) >}

        method contains_empty_terminal = first_terminals#contains_empty

        method extend_ft_map_with_current_rule : 'self =
          let current_rule_name =
            try
              path#top#name
            with Not_found -> 
                   failwith ("[Make.c#extend_ft_map_with_current_rule]: No current rule.")
        in {< first_terminals = first_terminals#extend_map_under (current_rule_name) >}

        method add_empty_terminal : 'self = 
          {< first_terminals = first_terminals#add_empty >}

        method remove_empty_terminal : 'self = 
          {< first_terminals = first_terminals#remove_empty >}

        method add_terminal (term : Terminal.Ordered.t) = 
          {< first_terminals = first_terminals#add (term) >}

        method clear_current_terminals : 'self = 
          {< first_terminals = first_terminals#clear >}


        val epsilon = new epsilon (sem_grm)

        method empty_rules : Rule.Name.Set.t = epsilon#rules
  
        method empty_rhs : (bool array) Rule.Name.Map.t = epsilon#rhs

        method get_epsilon = epsilon


        method update_with (other_continuation : 'self) = 
          {< 
            first_terminals = first_terminals#update_with (other_continuation#get_first_terminals);
            epsilon = other_continuation#get_epsilon
          >}

        method flag_empty_the_current_rhs : 'self = 
          let current_rule_name =
            try
              path#top#name
            with Path.Empty -> 
                   failwith ("[Make.c#flag_empty_the_current_rhs]: Empty path.") in
          let current_bit_pattern =
            try
               Rule.Name.Map.find (current_rule_name) (epsilon#rhs)
            with Not_found ->
                   failwith ("[Make.c#flag_empty_the_current_rhs]: No current rule.")
        in try
             current_bit_pattern.(rhs_index#get) <- true;
             {< epsilon = epsilon#add_rule (current_rule_name) >}
           with RHS_index.Undefined ->
                  failwith ("[Make.c#flag_empty_the_current_rhs]: No current rhs index.")
      end
    (*-----------------------------------------------------------------------------------*)

    (*----------------------------------*)

    type rhs_status = Inner | Outermost

    (*----------------------------------*)

    let rec check_basic_item (cont) = function
      AST.Non_Terminal (nt) -> check_rule (cont) (nt)
    | AST.Terminal (term)   -> cont#add_terminal (term)


    and check_extended_operator (cont) = function
      AST.Star (wrapped_production) ->
        let cont0 = check_prod (Inner) (cont) (wrapped_production#emb_prod)
        in cont0#add_empty_terminal

    | AST.Plus (wrapped_production) ->
        check_prod (Inner) (cont) (wrapped_production#emb_prod)

    | AST.List_Star (repetition) -> 
        let cont0 = check_rule (cont) (repetition#term)
        in cont0#add_empty_terminal

    | AST.List_Plus (repetition) ->
        check_rule (cont) (repetition#term)

    | AST.Option (wrapped_production) ->
        let cont0 = check_prod (Inner) (cont) (wrapped_production#emb_prod)
        in cont0#add_empty_terminal

    | AST.Brackets (wrapped_production) ->
        check_prod (Inner) (cont) (wrapped_production#emb_prod)   
        

    and check_inner_rhs (cont) (inner_rhs) =
      check_items (Inner) (cont) (inner_rhs#items)

    and check_item (cont) = 
      let cont0 = cont#clear_current_terminals
    in function 
         AST.Basic (bi)     -> check_basic_item (cont0) (bi)
       | AST.Extended (ext) -> check_extended_operator (cont0) (ext)


    and check_rhs (status) (cont) = function
      AST.Items (rhs) ->
        check_items (status) (cont) (rhs#items)
    | AST.Empty (loc) ->
        let cont0 = cont#add_empty_terminal
        in match status with
             Inner     -> cont0
           | Outermost -> cont0#flag_empty_the_current_rhs


    and check_items (status) (cont) = function
      item :: other_items ->
        let cont0 = check_item (cont) (item)
        in if   cont0#contains_empty_terminal
           then let cont1 = cont#update_with (cont0#remove_empty_terminal)
                in check_items (status) (cont1) (other_items)
           else cont#update_with (cont0)
    | [] -> 
        let cont0 = 
          match status with
            Inner     -> (* The current rhs is an inner one. *)
              cont           
          | Outermost -> (* The current rhs is outermost, hence the current rule derives *)
                         (* the empty word by this rhs. *)
              cont#flag_empty_the_current_rhs 
        in cont0#add_empty_terminal


    and check_prod (status) (cont) (prod) =
      let apply (cont) = check_rhs (status) (cont#incr_rhs_idx)
    in List.fold_left (apply) (cont) (prod#list_of_rhs)


    and check_rule (cont : #c) (nt : #AST.non_terminal) = 
      try
        let yet_computed_terminals = Terminal.First.find (nt#name) (cont#ft_map)
          (* Current rule was already checked: we simply return the yet computed terminals. *)
        in cont#set_terminals (yet_computed_terminals)
      with Not_found ->
             if   cont#mem_of_path (nt)
             then cont (* Left-recursion. *)
             else let rule = Core.Rule.Map_from_names.find (nt#name) (cont#rules) in
                    let cont0 = cont#clear_rhs_idx#clear_current_terminals in
                    let cont1 = check_prod (Outermost) (cont0#extend_path_with (nt)) (rule#get_rhs) in
                    let cont2 = cont1#extend_ft_map_with_current_rule
                  in cont#update_with (cont2)


    let result (core : #Core.Grammar.c) =
      let apply (rule_name) (rule : #Rule.c) (cont : #c) =
        let cont0 = check_rule (cont) (new AST.non_terminal (rule_name) (rule#name_loc))
      in cont0#clear_rhs_idx#clear_current_terminals#clear_path
    in Core.Rule.Map_from_names.fold (apply) (core#rules) (new c (core))

  end

(*--------------------------------------------------------------------------------------*)



(*--------------------------------------------------------------------------------------*)

class c (grm_AST : Core.AST.t) =
  (*-------------------------------------------------------*)

  let state : (#Continuation.c option) ref = ref (None) in

  (*-------------------------------------------------------*)


  (*----------------------------------------------*)
  
  let initialize_with (core : #Core.Grammar.c) = 
    state := Some (Continuation.result (core)) in

  (*----------------------------------------------*)


  (*------------------------------------------------------------------------------*)

  let cant_happen (method_name) =
    failwith ("[Base.c#" ^ method_name ^ "]: Initialization is missing.")

  (*------------------------------------------------------------------------------*)

in object (self)
     inherit Core.Grammar.c (grm_AST) as super

     (*-------------------------------------------------*)
     (* Each rule is mapped to its set of first tokens. *)

     method first_terminals : Terminal.First.t = 
       match !state with
         Some (s) -> s#ft_map
       | None     -> cant_happen ("first_terminals")

     (*-------------------------------------------------*)


     (*-----------------------------------------------------*)
     (* Each non-terminal is mapped to its set of following *)
     (* tokens (in rhs occurrences).                        *)

     (* method follow : Terminal.Follow.t *)

     (*-----------------------------------------------------*)


     (*--------------------------------------------*)
     (* The set of rules deriving the empty word.  *)

     method epsilon_rules : Rule.Name.Set.t = 
       match !state with
         Some (s) -> s#empty_rules
       | None     -> cant_happen ("epsilon_rules")

     (*--------------------------------------------*)


     initializer
       (*---------------------------*)
       (* Initializes and memoizes  *)

       initialize_with (super#copy)

       (*---------------------------*)
   end

(*--------------------------------------------------------------------------------------*)



(*---------------------------------------*)
(* Double recursive rules (=> ambiguity) *)
(*---------------------------------------*)


(*--------*)
(* Follow *)
(*--------*)


(*------------------------*)
(* Reduction of a grammar *)
(*------------------------*)


(*-----------------------------*)
(* Unique identifier generator *)
(*-----------------------------*)

let (gen_sym, clr_sym) =
  let r = ref (0) 
in ((fun () -> r := !r + 1; "#" ^ string_of_int (!r)), 
     fun () -> r := 0)



(*
let generic_iterator (reduction) (unreduced) =
  let aux (x) (x_acc, rule_acc) =
    let (reduced_x, reduced_new_rules) = reduction (x)
  in (reduced_x :: x_acc, Rule.Map_from_names.union (reduced_new_rules) (rule_acc))
in List.fold_right (aux) (unreduced) ([], Rule.Map_from_names.empty)


let make_ghost_non_terminal (name) =
  AST.Basic (AST.Non_Terminal (name, Location.none))


let rec unfold_star (AST.Inner_rhs {AST.rhs = (rm, loc)}) =
  let new_rule_name = gen_sym () in
  let non_terminal_token = 
    make_ghost_non_terminal (new_rule_name) in
  let new_rhs = AST.Prod [
                          AST.Items {
                            AST.rhs = (rm @ [non_terminal_token], Location.none);
                            AST.item_idx = 0
                          };
                          AST.Empty (Location.none)
                         ] in
  let new_rule = new Rule.c (new_rhs) (Location.none) in
  let (reduced_new_rule, new_rules) = reduce_rule (new_rule)
in (non_terminal_token, Rule.Map_from_names.add (new_rule_name) (reduced_new_rule) (new_rules))


and unfold_plus (AST.Inner_rhs {AST.rhs = (rm, loc)}) =
  let new_rule_name = gen_sym () in
  let non_terminal_token = make_ghost_non_terminal (new_rule_name) in
  let new_rhs = 
      AST.Prod [
                 AST.Items {
                   AST.rhs = (rm @ [AST.Extended (AST.Star (AST.Inner_rhs { 
                                                               AST.rhs = (rm, loc);
                                                               AST.item_idx = 0
                                                            }, 
                                                            Location.none
                                                           )
                                                 )
                                   ], 
                              Location.none
                             );
                  AST.item_idx = 0
                 }
                ] in
  let new_rule = new Rule.c (new_rhs) (Location.none) in
  let (reduced_new_rule, new_rules) = reduce_rule (new_rule)
in (non_terminal_token, Rule.Map_from_names.add (new_rule_name) (reduced_new_rule) (new_rules))


and unfold_option (AST.Inner_rhs {AST.rhs = (items, loc)}) =
  let new_rule_name = gen_sym () in
  let non_terminal_token = make_ghost_non_terminal (new_rule_name) in
  let new_rhs = AST.Prod [
                          AST.Items {AST.rhs = (items, loc); AST.item_idx = 0};
                          AST.Empty (Location.none)
                         ] in
  let new_rule = new Rule.c (new_rhs) (Location.none) in
  let (reduced_new_rule, new_rules) = reduce_rule (new_rule)
in (non_terminal_token, Rule.Map_from_names.add (new_rule_name) (reduced_new_rule) (new_rules))
     

and unfold_list_plus (non_term) (term) =
  let new_rule_name = gen_sym () in
  let non_terminal_token = make_ghost_non_terminal (new_rule_name) in
  let new_rhs = 
      AST.Prod [
                 AST.Items {
                   AST.rhs = ([ 
                               AST.Basic (AST.Non_Terminal (non_term)); 
                               AST.Extended (AST.Star (AST.Inner_rhs {
                                                         AST.rhs = ([
                                                                     AST.Basic (AST.Terminal (term)); 
                                                                     AST.Basic (AST.Non_Terminal (non_term))
                                                                    ],
                                                                    Location.none
                                                                   );
                                                         AST.item_idx = 0
                                                       }, 
                                                       Location.none
                                                      )
                                            )
                              ],
                              Location.none
                             );
                  AST.item_idx = 0
                 }
                ] in
  let new_rule = new Rule.c (new_rhs) (Location.none) in
  let (reduced_new_rule, new_rules) = reduce_rule (new_rule)
in (non_terminal_token, Rule.Map_from_names.add (new_rule_name) (reduced_new_rule) (new_rules))

     
and unfold_list_star (non_term) (term) =
  let new_rule_name = gen_sym () in
  let non_terminal_token = make_ghost_non_terminal (new_rule_name) in
  let new_rhs = AST.Prod [
                          AST.Items {
                            AST.rhs = ([AST.Extended (AST.List_Plus (non_term, term, Location.none))], 
                                       Location.none
                                      );
                            AST.item_idx = 0
                          };
                          AST.Empty (Location.none)
                         ] in
  let new_rule = new Rule.c (new_rhs) (Location.none) in
  let (reduced_new_rule, new_rules) = reduce_rule (new_rule)
in (non_terminal_token, Rule.Map_from_names.add (new_rule_name) (reduced_new_rule) (new_rules))
     

and reduce_extended (comp) = 
  let new_rule_name = gen_sym () in
  let non_terminal_token = make_ghost_non_terminal (new_rule_name)
in match comp with
     AST.Star (rm, loc)                  -> unfold_star (rm)
   | AST.Plus (rm, loc)                  -> unfold_plus (rm)
   | AST.List_Plus (non_term, term, loc) -> unfold_list_plus (non_term) (term)
   | AST.List_Star (non_term, term, loc) -> unfold_list_star (non_term) (term)
   | AST.Option (rm, loc)                -> unfold_option (rm)


and reduce_token = function
  AST.Extended (c) -> reduce_extended (c)
| t               -> (t, Rule.Map_from_names.empty)


and reduce_outermost_rhs = function
  AST.Items {AST.rhs = (rm, loc)} ->
    let (tokens, new_rules) = generic_iterator (reduce_token) (rm)
    in (AST.Items {
          AST.rhs = (tokens, Location.none);
          AST.item_idx = 0
        }, 
        new_rules
       )
| AST.Empty (loc) -> 
    (AST.Empty (loc), First.empty)

and reduce_prod (AST.Prod (p)) = 
  let (items, new_rules) = generic_iterator (reduce_outermost_rhs) (p)
in (AST.Prod (items), new_rules) 


and reduce_rule (rule) =
  let (p', new_rules) = reduce_prod (rule#get_rhs)
in (rule#set_rhs (p'), new_rules)

let reduce (grm) =
  let aux (rule_name) (rule) (acc) =
    let (rule', new_rules) = reduce_rule (rule) in
    let acc0 = Rule.Map_from_names.add (rule_name) (rule') (acc)
  in Rule.Map_from_names.union (acc0) (new_rules) in
  let rules = Rule.Map_from_names.fold (fun name cont acc -> (AST.Rule ((name, cont#name_loc), cont#get_rhs))::acc) 
                         (Rule.Map_from_names.fold (aux) (grm.rules) (Rule.Map_from_names.empty)) [] in
  let tokens = TokenMap.fold (fun token loc acc -> (token, loc)::acc) (grm.tokens) [] in
  let ast_grm = new AST.t (tokens) (rules) (grm.axiom) (grm.file)
in make (ast_grm)
*)
end
