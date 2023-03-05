module type S =
  sig
    type production

    class type rule_ct =
      object ('self)
        val rhs         : production
        method get_rhs  : production
        method name_loc : Location.t
        method set_rhs  : production -> 'self
      end 

    class rule : production -> Location.t -> rule_ct

    class c : production -> Location.t -> rule_ct

    (* Module [RulenameMap] is a map whose domain is of type [string]. *)
    (* Remember [Ast.rule_name] is a type abbreviation for [string].   *)

    module type NameSig =
      sig
        module Map : Standard.String.MapSig
        module Set : Standard.String.SetSig
      end

    module Name : NameSig

    (* Module [Rules] is a map from rule names ([RulenameMap]) to *)
    (* [rule] objects.                                            *)

    module type Map_from_namesSig =
      sig
        type name = Name.Map.key
        type t = (rule) Name.Map.t

        val empty  : t
        val add    : name -> rule -> t -> t
        val find   : name -> t -> rule
        val remove : name -> t -> t
        val mem    : name -> t -> bool
        val iter   : (name -> rule -> unit) -> t -> unit
        val map    : (rule -> rule) -> t -> t
        val fold   : (name -> rule -> 'a -> 'a) -> t -> 'a -> 'a

        val union  : t -> t -> t
      end


    module Map_from_names : Map_from_namesSig

    (* Module [Set] is a set of rule names. *)

    module type SetSig = Standard.String.SetSig

    module Set : SetSig

end

module Make (Prod : sig type production end) =
  struct
    type production = Prod.production

class type rule_ct =
  object ('self)
    val rhs         : production
    method get_rhs  : production
    method name_loc : Location.t
    method set_rhs  : production -> 'self
  end 

class rule (prod : production) (loc : Location.t) : rule_ct =
  object
    method name_loc = loc
    val rhs = prod
    method get_rhs = rhs
    method set_rhs (p) = {< rhs = p >}
  end

class c = rule

(* Module [Rules] is a map from rule names ([StringMap.key]) *)
(* to [rule] objects. This module is necessary in order to   *)
(* achieve explicit monomorphism on map co-domain, but one   *)
(* could perfectly use a polymorphic [StringMap] instead.    *) 

module type NameSig =
  sig
    module Map : Standard.String.MapSig
    module Set : Standard.String.SetSig
  end

module Name : NameSig =
  struct
    module Map = Standard.String.Map
    module Set = Standard.String.Set
  end


module type Map_from_namesSig =
  sig
    type name = Name.Map.key
    type t = (rule) Name.Map.t

    val empty  : t
    val add    : name -> rule -> t -> t
    val find   : name -> t -> rule
    val remove : name -> t -> t
    val mem    : name -> t -> bool
    val iter   : (name -> rule -> unit) -> t -> unit
    val map    : (rule -> rule) -> t -> t
    val fold   : (name -> rule -> 'a -> 'a) -> t -> 'a -> 'a

    val union  : t -> t -> t
  end

module Map_from_names : Map_from_namesSig =
  struct
    type key = Name.Map.key

    type name = key
    type t = (rule) Name.Map.t

    let empty  = Name.Map.empty
    let add    = Name.Map.add
    let find   = Name.Map.find
    let remove = Name.Map.remove
    let mem    = Name.Map.mem
    let iter   = Name.Map.iter
    let map    = Name.Map.map
    let fold   = Name.Map.fold

    let union  = fold (add)
  end

  (* Module [Set] is a synonym for the module [StringSet], *)
  (* and denotes sets of rule names.                             *)

  module type SetSig = Standard.String.SetSig

  module Set : SetSig = Standard.String.Set

end
