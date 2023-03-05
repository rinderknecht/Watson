module Ordered :
  sig
    type t = Core.AST.terminal
    val compare : Core.AST.terminal -> Core.AST.terminal -> int
    val empty : Core.AST.terminal
  end

module Set : Set.S with type elt = Ordered.t

module type Map_from_rule_namesSig =
  sig
    type rule_name = Core.Rule.Name.Map.key
    type t = (Set.t) Core.Rule.Name.Map.t

    val empty  : t
    val add    : rule_name -> Set.t -> t -> t
    val find   : rule_name -> t -> Set.t
    val remove : rule_name -> t -> t
    val mem    : rule_name -> t -> bool
    val iter   : (rule_name -> Set.t -> unit) -> t -> unit
    val map    : (Set.t -> Set.t) -> t -> t
    val fold   : (rule_name -> Set.t -> 'a -> 'a) -> t -> 'a -> 'a
  end

module Map_from_rule_names : Map_from_rule_namesSig 

module First : Map_from_rule_namesSig 

module Follow : Map_from_rule_namesSig 

