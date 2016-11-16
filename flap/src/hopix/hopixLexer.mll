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

let constr_id = (upper_letter | '_') (letter | digit | '_')*

let type_variable = '\'' lower_letter (letter | digit | '_')*

let int =
    digit+
  | '0' ['x' 'X'] (digit | ['a'-'f' 'A'-'F'])+
  | '0' ['o' 'O'] ['0'-'7']+
  | '0' ['b' 'B'] ['0' '1']+

let char = '\'' (atom | '"') '\''

let string = '"' (atom | ('\\' '"'))* '"'
  
rule token =
  parse
  (** Layout *)
  | newline                        { next_line_and token lexbuf }
  | blank+                         { token lexbuf               }
  | "val"                          { VAL }
  | "="                            { EQUALS }
  | "("                            { LPARAN }
  | ")"                            { RPARAN }
  | ","                            { COMMA }
  | ":"                            { COLON }             
  | "->"                           { ARROW }
  | id                             { ID (Lexing.lexeme lexbuf) }
  | type_variable                  { TYPE_VARIABLE (Lexing.lexeme lexbuf) }
  | alien_prefix_id                { PREFIX_ID (Lexing.lexeme lexbuf) }             
  | int                            { INT (Int32.of_string (Lexing.lexeme lexbuf)) }
  | char                           { CHAR (Lexing.lexeme lexbuf).[0] }
  | string                         { STRING (Lexing.lexeme lexbuf) } 
  | eof                            { EOF       }
  (** comment **)
  | "{-"                           { treat_comment_v1 1 lexbuf }  
  | "--"                           { treat_comment_v2 lexbuf }
  (** Lexing error. *)
  | _                              { error lexbuf "unexpected character." }
and treat_comment_v1 nested_level = 
  parse
  | "{-" { treat_comment_v1 (nested_level+1) lexbuf }
  | "-}" { if nested_level = 1 then token lexbuf else treat_comment_v1 (nested_level-1) lexbuf }
  | eof  { error lexbuf "You forgot to close a comment with \"-}\""}
  | _    { treat_comment_v1 nested_level lexbuf }
and treat_comment_v2 =
  parse
  | newline { token lexbuf }
  | eof     { EOF }
  | _       { treat_comment_v2 lexbuf }
