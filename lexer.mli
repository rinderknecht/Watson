    val eof : Parser.token

    (* \subsection{Lexing functionalities} *)

    (* 
      The call [get_token (tr) (file) (lexbuf)] returns a token from
      the lexing buffer [lexbuf]. It is assumed that this buffer
      corresponds to file [file]. According to the value of [tr] a
      trace may be done. 
    *)

    val get_token : ?trace:out_channel -> Lexing.lexbuf -> Parser.token

    val standalone : src:string -> unit

    (* \subsection{Errors} *)

    (* The type [error], together with the exception [Error], captures
       all the possible errors that may occur during the lexing of a 
       grammar:
       \begin{itemize}
         \item [Illegal_keyword (id, loc)] means that an unknown
               identifier [id] was intended as a keyword
               (ie. preceeded by a \texttt{\%}), at source location
               [loc];
         \item [Illegal_character (c, loc)] means that an illegal
               character [c] (represented here by a string) was
               encountered at source location [loc];
         \item [Keyword_expected (loc)] means that a keyword was
               expected at source location [loc];
         \item [Unterminated_string (loc)] means that a string (ie. an
               excerpt of concrete syntax) was opened at location
               [loc], but not closed;
         \item [Unterminated_comment (loc)] means that a comment
               started at location [loc] but never closed.
       \end{itemize}
    *)

    type error =
      Illegal_keyword      of string * Location.t
    | Illegal_character    of string * Location.t
    | Keyword_expected     of Location.t
    | Unterminated_string  of Location.t
    | Unterminated_comment of Location.t

    exception Error of error

    (* 
      The exception [Fatal_error] is raised whenever a fatal error
      occurs (hence its name) during lexing. These errors are critical
      ones, generally urging the user of the lexer to stop.
    *)

    type message = string

    exception Fatal_error of message


