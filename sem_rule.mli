(* A [rule] is the semantic version of the [AST.rule] type.         *)
(* The class takes two arguments. The first one is a                *)
(* syntactic production (ie. a value ofg type [AST.production])     *)
(* and the second is the location of the associated rule name       *)
(* (ie. a value of type [Location.t]).                              *)
(* The object provides the following methods:                       *)
(*   1) [get_rhs] returns the first value parameter of the class.   *)
(*   2) [name_loc] returns the second value parameter of the class. *)
(*   3) [set_rhs (new_rhs)] returns a functional copy of the object *)
(*      whose right-hand side ([rhs]) is now [new_rhs].             *)

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

module Make (Prod : sig type production end) : S with type production = Prod.production






