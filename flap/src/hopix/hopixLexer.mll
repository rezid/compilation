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
             
let int =
    digit+
  | '0' ['x' 'X'] (digit | ['a'-'f' 'A'-'F'])+
  | '0' ['o' 'O'] ['0'-'7']+
  | '0' ['b' 'B'] ['0' '1']+ 
let var_id = lower_letter (letter | digit | '_')*
    
rule token =
  parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }
  | "val"           { VAL }
  | "="             { EQUALS }             
  | var_id          { VAR_ID (Lexing.lexeme lexbuf) }
  | int             { INT (Int32.of_string (Lexing.lexeme lexbuf)) }                     
  | eof             { EOF       }
  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }

