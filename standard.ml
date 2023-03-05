module Stack =
  struct

(* \subsubsection{Using modules and objects} *)

(* See section~\ref{spec_fun_stack_with_modules} for comments on
   [EltSig], [S] and [Make].  
*)

module type EltSig = 
  sig
    type t
    val equal : t -> t -> bool
  end

module type S = 
  sig
    type elt

    exception Empty

    class c :
      object ('self)
        val repr      : elt list
        method mem    : elt -> bool
        method clear  : 'self
        method length : int
        method pop    : elt * 'self
        method push   : elt -> 'self
        method top    : elt
      end
  end

module Make (Elt : EltSig) =
  struct

    type elt = Elt.t

    exception Empty

    class c =
      object
        val repr = ([] : Elt.t list)

        method mem (elt : Elt.t) = List.exists (Elt.equal (elt)) (repr)
 
        method push (elt : Elt.t) = {< repr = elt :: repr >}

        method pop =
          match repr with
            []           -> raise (Empty)
          | head :: tail -> (head, {< repr = tail >})

        method top =
          match repr with
            []           -> raise (Empty)
          | head :: tail -> head

        method clear = {< repr = [] >}

        method length = List.length (repr)
      end
  end


(* \subsubsection{Using objects only} *)

(* 
  See section~\ref{spec_fun_stack_with_objects} for comments on
  [Empty], [['elm] core] and [['elm] base].
*)

exception Empty

class ['a] core =
  object
    val repr = ([] : 'a list)

    method push (elm) = {< repr = elm :: repr >}

    method pop =
      match repr with
        []           -> raise (Empty)
      | head :: tail -> (head, {< repr = tail >})

    method top =
      match repr with
        []           -> raise (Empty)
    | head :: tail -> head

    method clear = {< repr = [] >}

    method length = List.length (repr)
  end


class ['elt] base =
  object
    constraint 'elt = <equal : 'elt -> bool; ..>

    inherit ['elt] core

    method mem (elt : 'elt) = List.exists (elt#equal) (repr)
  end

end


(* Customized I/O definitions *)

(* This module provides some very basic features for file handling,*)
(* and its related errors. *)

(* Basic data structures. *)

module IO = 
  struct

    class type in_handler_ct = 
      object
	method filename : string
	method channel  : in_channel
	method close    : unit
      end

    class in_handler ~filename:(f_name) =
      object
	val chan =
	  try
	    Pervasives.open_in (f_name)
	  with Sys_error (msg) -> 
            begin
              prerr_endline ("Watson I/O error: Unable to open file " ^ f_name ^ " for input.");
              exit (-1)
            end

	method filename = f_name;
	method channel = chan
	method close = Pervasives.close_in (chan)
      end
	
    class type out_handler_ct = 
      object
	method filename : string
	method channel  : out_channel
	method close    : unit
      end

    class out_handler ~filename:(f_name) =
      object
	val chan = 
	  try
	    Pervasives.open_out (f_name)
	  with Sys_error (msg) -> 
            begin
              prerr_endline ("Watson I/O error: Unable to open file " ^ f_name ^ " for output.");
              exit (-1)
            end
	      
	method filename = f_name;
	method channel = chan
	method close = Pervasives.close_out (chan)
      end

end

(* 
  Module [String] extends homonym module of the standard library with
  string sets and maps.
*)

module String =
  struct

    module Ordered =
      struct
        type t = string
        let compare = Pervasives.compare
     end

    module type SetSig = (Set.S with type elt = Ordered.t)

    module Set = Set.Make (Ordered)

    module type MapSig = (Map.S with type key = Ordered.t)

    module Map = Map.Make (Ordered) 

  end

