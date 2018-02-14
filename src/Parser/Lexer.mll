{
  open Lexing
  open Error
  open Position
  open Parser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)

}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']

let lowercase_alpha = ['a'-'z' '_']

let uppercase_alpha = ['A'-'Z' '_']

let alpha = lowercase_alpha | uppercase_alpha

let alphanum = alpha | digit | '_'

let identifier = alpha alphanum*

let function   = "+" | "*" | "/" | "-" | "fst" | "snd" | "fix"

rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }

  (** Keywords *)
  | "fun"           { FUN  }
  | "->"            { TO   }
  | "let"           { LET  }
  | "in"            { IN   }
  | "="             { EQ   }

  (** Identifiers *)
  | identifier as i { ID i }

  (** constant *)
  (** int *)
  | digit+ as d     { INT (int_of_string d) }
  (** boolean *)
  | "true"          { BOOL(true)  }
  | "false"         { BOOL(false) }
  (** function operators *)
  | function as f   { FCT f }

  (** Punctuation *)
  | ","             { COMMA     }
  | "("             { LPAREN    }
  | ")"             { RPAREN    }
  | eof             { EOF       }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }