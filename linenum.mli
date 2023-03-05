
(*
  This module provides an auxiliary lexer for determining the line
  number corresponding to a given absolute location in a file (i.e. a
  character position counted from the beginning of the file), together
  with the absolute location of the beginning of that line. This
  module is intended to be mainly used when an error condition is
  reached during the execution and the programme needs to be stoppped
  with a message. See module [Location] for further informations about
  locations and pretty-printing of such messages.
*)

(* A REVOIR
  The call [for_position (file) (loc)] returns a record of two integers
  characterising  the absolute location [loc] (i.e. the number of
  characters from the beginning of the file minus one --- since the
  first caracter has location 0) in the file named [file]. The field
  [line] is the line number corresponding to [loc] in that source
  file. The field [char] is the absolute location of the
  \emph{beginning} of that line in [file].
*)

class relative_loc :
  filename:string -> abs_loc:int ->
  object
    method line_num  : int
    method line_start : int
  end

