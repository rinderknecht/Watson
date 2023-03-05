
(*
  This module provides an auxiliary lexer for determining the line
  number corresponding to a given absolute location in a file (i.e. a
  character position counted from the beginning of the file (0)),
  together with the absolute location of the beginning of that line
  (first line is counted 1). This module is intended to be mainly used
  when an error condition is reached during the execution and the
  programme needs to be stoppped with a message. See module [Location]
  for further informations about locations and pretty-printing of such
  messages.
*)

{
}

(*
  Only one lexing rule is declared: [skip_lines]. It takes two
  argument: [loc] is the absolute location, and [line_num] is the
  current line number. 

  The first lexing clause scans a line. If the line ends before [loc]
  then a recursive call is made, with an incremented line
  number. Otherwise, the clause returns the pair made of the current
  line number and the line start (i.e. the absolute position of the
  line start).

  The second lexing clause matches the case of the last line of the
  input stream. The pair made of the current line number and the line
  start is returned.
*)

rule skip_lines loc line_num = parse
  [^ '\n' '\r']* ('\n' | '\r' | "\r\n")
    { 
     let line_end = Lexing.lexeme_end (lexbuf)
     in if   line_end <= loc
        then skip_lines (loc) (line_num + 1) (lexbuf)
        else let line_start = Lexing.lexeme_start (lexbuf)
             in (line_num, line_start)
    }
| [^ '\n' '\r']* eof
    { 
     let line_start = Lexing.lexeme_start (lexbuf)
     in (line_num, line_start)
    }

{
(*
  The class [relative_loc] captures the concept of relative location,
  i.e. locations in a source code referenced by a pair made of the
  line number (first line is numered 1) and absolute position of this
  line start, i.e. the number of characters from the beginning of the
  file.

  It takes two arguments: [fname] is the source filename, and [loc] is
  the absolute location in this source file, i.e. the number of
  characters counted from the beginning of the file. 

  The initialization of the class is simply a call to the lexer rule
  [skip_lines] (see above) on the source file.

  The two public methods are [line_num] and [line_start], and are
  straight-forward.
*)

class relative_loc ~filename:(fname) ~abs_loc:(loc) =
  let in_chan = open_in_bin (fname) in
  let lexbuf = Lexing.from_channel (in_chan) in
  let (line_num, line_start) = skip_lines (loc) (1) (lexbuf) in
  let () = close_in (in_chan)
in object
     method line_num   = line_num
     method line_start = line_start
   end
}
