
(* 
  This signature provides some useful modules that are not included in
  the \OCaml standard library --- hence its name.
*)


(*
  \subsection{Stack} 
  Module [Stack] provides two straight-forward ways of defining
  functional stacks (the module [Stack] of the \OCaml standard library
  is based on a destructive implementation).
*)

module Stack :
  sig

(* 
  \subsubsection{Using modules and objects\label{spec_fun_stack_with_modules}}
*)

(* 
  The module type [EltSig] specifies the signature for the stack
  elements. The abstract type [t] stands for the element type and an
  equality between those elements is required by the function
  [equal].
*)

    module type EltSig = 
      sig
        type t
        val equal : t -> t -> bool
      end


(* The module type [S] is the signature for the stack environment. It
   provides:
   \begin{itemize}
      \item an abstract type [elt] for the stack elements;
      \item an exception [Empty] raised when an operation is forbidden
            on an empty stack (see methods below);
      \item a class [c] standing for the stack type itself. It
            contains:
            \begin{itemize}
              \item an instance variable [repr] denoting the internal
                    representation of the stack: a list of elements;
              \item a method [mem] which is the belonging predicate
                    for the stack;
              \item a method [clear] returning an empty stack of the
                    same class;
              \item a method [length] which provides the size of the
                    stack (ie. number of elements);
              \item a method [top] returning the elements on top of
                    the stack (exception [Empty] is raised if the
                    stack was empty); 
              \item a method [pop] giving the top of the stack and a
                    new stack equal to the current object without the
                    top (exception [Empty] is raised if the
                    stack was empty);
              \item a method [push] taking an element and returning a
                    new stack equal to the current object with the new
                    element on the top.
            \end{itemize}

   \end{itemize}
*)

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
            method top    : elt
            method pop    : elt * 'self
            method push   : elt -> 'self
          end
      end


(* 
  The functor [Make] builds a functional-stack module for a given
  element module: it takes a module [Elt] (of signature [EltSig]) and
  returns a module whose signature matches [S] where the type [elt]
  of the stack elements equals the type [t] of [Elt].
*)

    module Make (Elt : EltSig) : (S with type elt = Elt.t)



(* \subsubsection{Using objects only\label{spec_fun_stack_with_objects}} *)

(* 
  The exception [Empty] is raised by those of the methods below which
  cannot operate on empty stacks. 
*)

    exception Empty

(* The parametric class [['elm] core] stands for the stack. It
   contains:
   \begin{itemize}
     \item an instance variable [repr] denoting the internal
           representation of the stack: a list of elements;
     \item a method [clear] returning an empty stack of the
           same class;
     \item a method [clear] returning an empty stack of the
           same class;
     \item a method [length] which provides the size of the
           stack (ie. number of elements);
     \item a method [top] returning the elements on top of
           the stack (exception [Empty] is raised if the
           stack was empty); 
     \item a method [pop] giving the top of the stack and a
           new stack equal to the current object without the
           top (exception [Empty] is raised if the
           stack was empty);
     \item a method [push] taking an element and returning a
           new stack equal to the current object with the new
           element on the top.
   \end{itemize}

   The main difference with previous stack
   specification~\ref{spec_fun_stack_with_modules} is that parametricity is
   here achieved throughout \emph{class parametrisation} instead of
   \emph{functorisation}. Equality is provided in class [['elm] base]
   below.
*)

    class ['elm] core :
      object ('self)
        val repr      : 'elm list
        method clear  : 'self
        method length : int
        method top    : 'elm
        method pop    : 'elm * 'self
        method push   : 'elm -> 'self
      end


(* 
  The class [['elm] base] is a subclass of [['elm] core] with
  equality on stack elements. 
*)

    class ['elt] base :
      object
        constraint 'elt = <equal : 'elt -> bool; ..>

        inherit ['elt] core

        method mem : 'elt -> bool
      end
end (* module Stack *)



(* 
  \subsection{IO} 

  Customized I/O definitions 
*)


module IO :
  sig

(*i
    type +'a file

    val open_in  : ~filename:string -> [`Read] file
    val open_out : ~filename:string -> [`Write] file

    val channel_in : [`Read] file -> in_channel
    val channel_out : [`Write] file -> out_channel

    val close_in : [`Read] file -> unit
    val close_out : [`Write] file -> unit

    val filename : [`Read | `Write] file -> string
i*)    
    

(*
  This module provides some very basic features for file handling,
  and its related errors. 
*)  

    class type in_handler_ct = 
      object
        method filename : string
        method channel  : in_channel
        method close    : unit
      end

    class in_handler : filename:string -> in_handler_ct

    class type out_handler_ct = 
      object
        method filename : string
        method channel  : out_channel
        method close    : unit
      end

    class out_handler : filename:string -> out_handler_ct

  end

(*
  \subsection{String} 
  Module [String] extends homonym module of the standard library with
  string sets and maps.
*)

module String :
  sig

    module Ordered : 
      sig 
        type t = string 
        val compare : t -> t -> int 
      end

    module type SetSig = (Set.S with type elt = Ordered.t)

    module Set : SetSig

    module type MapSig = (Map.S with type key = Ordered.t)

    module Map : MapSig
  end
