{
  open Lexing
  open Error
  open Position
  open HopixParser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)

  (* Function qui permet de transformer un string (contient un character échappé) en char  
     ** op = 0 le string represente un charactere complet y compris les apostrophe delimitant
     ** op = 1 le string est un atom (sans les apostrophe delimitant)
  *)
  let to_char ?(op=0) s = 
    if s.[1-op] != '\\' then s.[1-op] else
      match s.[2-op] with
      | 'n' -> '\n'
      | 't' -> '\t'
      | 'b' -> '\b'
      | 'r' -> '\r'
      | '\'' -> '\''
      | '\\' -> '\\'
      | _ -> if op = 0 then
          Char.chr (int_of_string (String.sub s 2 (String.length s - 3)))
        else
          Char.chr (int_of_string (String.sub s 1 (String.length s - 1)))   
}

let newline = ('\010' | '\013' | "\013\010")
              
let blank   = [' ' '\009' '\012']
              
let digit = ['0'-'9']
            
let upper_letter = ['A' - 'Z']
                   
let lower_letter = ['a' - 'z']
                   
let letter = upper_letter | lower_letter
             
let op = ['+' '-' '*' '/' '<' '>' '=']
    
let printable_modified = ['\035'-'\126'] | '\032' | '\033'

let atom =
    '\\' ((['0'-'1'] ['0'-'9'] ['0'-'9']) | ('2' ['0'-'5'] ['0'-'5']))
  | '\\' '0' ['x' 'X'] (digit | ['a'-'f' 'A'-'F']) (digit | ['a'-'f' 'A'-'F'])
  | '\\' '0' ['o' 'O'] ['0'-'7']+
  | '\\' '0' ['b' 'B'] ['0' '1']+
  | printable_modified
  | '\\' ['\'' '\\' 'n' 't' 'b' 'r']
 
let alien_prefix_id = '`' (letter | digit | op | '_')+

let alien_infix_id = alien_prefix_id '`'

let id = lower_letter (letter | digit | '_')*

let constr_id = (upper_letter) (letter | digit | '_')*
                | '_' (letter | digit | '_')+                         

let type_variable = '\'' lower_letter (letter | digit | '_')*

let int =
    digit+
  | '0' ['x' 'X'] (digit | ['a'-'f' 'A'-'F'])+
  | '0' ['o' 'O'] ['0'-'7']+
  | '0' ['b' 'B'] ['0' '1']+

let char = '\'' (atom | '"') '\''
  
rule token =
  parse
  (** Layout *)
  | newline                        { next_line_and token lexbuf }
  | blank+                         { token lexbuf               }
  | eof                            { EOF                        }

  (** Keywords *)
  | "val"                          { VAL    }
  | "fun"                          { FUN    }
  | "and"                          { AND    }
  | "if"                           { IF     }
  | "then"                         { THEN   }
  | "elif"                         { ELIF   }
  | "else"                         { ELSE   }
  | "while"                        { WHILE  }
  | "ref"                          { REF    }
  | "extern"                       { EXTERN }
  | "type"                         { TYPE   } 
   
  (** Punctuation *)         
  | ":="                           { AFFECT           }
  | "_"                            { UNDERSCORE       }
  | "("                            { LPARAN           }
  | ")"                            { RPARAN           }
  | "["                            { L_SQUARE_BRACKET }
  | "]"                            { R_SQUARE_BRACKET }
  | "}"                            { R_CURLY_BRACKET  }
  | "!"                            { EXCLAMATION      }
  | "?"                            { QUESTION_MARK    }
  | "|"                            { BAR              }
  | "&"                            { AMPERSAND        }
  | "{"                            { L_CURLY_BRACKET  }
  | ","                            { COMMA            }
  | ":"                            { COLON            }             
  | ";"                            { SEMICOLON        }
  | "->"                           { ARROW            }
  | "=>"                           { DOUBLE_ARROW     }
  | "\\"                           { ANTISLASH        }

  (** Operators *)
  | "+"                            { PLUS }
  | "-"                            { MOIN }
  | "*"                            { STAR }
  | "/"                            { DIV  }
  | "<"                            { LT }
  | "<="                           { LE }
  | ">"                            { GT }
  | ">="                           { GE }
  | "||"                           { B_OR   }
  | "&&"                           { B_AND  }
  | "="                            { EQUALS }

  (** Identifiers *)
  | id                             { ID (Lexing.lexeme lexbuf)            }
  | type_variable                  { TYPE_VARIABLE (Lexing.lexeme lexbuf) }
  | alien_prefix_id                { PREFIX_ID (Lexing.lexeme lexbuf) }
  | alien_infix_id                 { INFIX_ID (Lexing.lexeme lexbuf)  }
  | constr_id                      { CONSTR_ID (Lexing.lexeme lexbuf) }
  
   (** Literals *)
  | int                            { INT (Int32.of_string (Lexing.lexeme lexbuf)) }
  | char                           { CHAR (to_char (Lexing.lexeme lexbuf))        }
  | "\""                           { treat_string "" lexbuf } 

  (** comment **)
  | "{-"                           { treat_comment_v1 1 lexbuf }  
  | "--"                           { treat_comment_v2 lexbuf   }

  (** Lexing error. *)
  | _                              { error lexbuf "unexpected character." }

(** parsing comment of type {* ... *} *)
and treat_comment_v1 nested_level = 
  parse
  | "{-" { treat_comment_v1 (nested_level+1) lexbuf }
  | "-}" { if nested_level = 1 then token lexbuf else treat_comment_v1 (nested_level-1) lexbuf }
  | eof  { error lexbuf "You forgot to close a comment with \"-}\""}
  | _    { treat_comment_v1 nested_level lexbuf }

(** parsing comment of type -- *)
and treat_comment_v2 =
  parse
  | newline { token lexbuf }
  | eof     { EOF }
  | _       { treat_comment_v2 lexbuf }

(** parsing string litterals *)
and treat_string str =
  parse
  | "\\\""  { treat_string (str ^ "\"") lexbuf }
  | atom    { treat_string (str ^ String.make 1 (to_char ~op:1 (Lexing.lexeme lexbuf))) lexbuf }
  | "\""    { STRING str }
  | eof     { error lexbuf "You forgot to close the string with \"."}
  | _       { error lexbuf "Unexpected character in string." }
