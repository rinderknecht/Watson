(*---------------------------------------------------------------------------*)
(* Module definitions for First and Follow functions.                        *)

module Ordered = 
  struct
    type t = Core.AST.terminal
    let compare (t0) (t1) = Pervasives.compare (t0#ident) (t1#ident)
    let empty = new Core.AST.terminal ("%empty") (Location.none)
  end

module Set = Set.Make (Ordered)

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

module Map_from_rule_names : Map_from_rule_namesSig =
  struct
    type key = Core.Rule.Name.Map.key

    type rule_name = key
    type t = (Set.t) Core.Rule.Name.Map.t

    let empty  = Core.Rule.Name.Map.empty
    let add    = Core.Rule.Name.Map.add
    let find   = Core.Rule.Name.Map.find
    let remove = Core.Rule.Name.Map.remove
    let mem    = Core.Rule.Name.Map.mem
    let iter   = Core.Rule.Name.Map.iter
    let map    = Core.Rule.Name.Map.map
    let fold   = Core.Rule.Name.Map.fold
  end

module First : Map_from_rule_namesSig = Map_from_rule_names

module Follow : Map_from_rule_namesSig = Map_from_rule_names

(*---------------------------------------------------------------------------*)
