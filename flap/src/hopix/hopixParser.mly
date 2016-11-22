%{
    open HopixAST

    (** Function for testiong purpose, it's create a dummy position*)       
    let dummy_pos s =
      Position.with_poss Lexing.dummy_pos Lexing.dummy_pos (Id s)      
%}
   
%token EOF
%token<Int32.t> INT
%token<char> CHAR                  
%token<string> ID
%token<string> STRING                              
%token<string> PREFIX_ID
%token<string> INFIX_ID
%token<string> CONSTR_ID                                 
%token<string> TYPE_VARIABLE                 
%token VAL
%token FUN                      
%token AND
%token COLON
%token LPARAN                  
%token RPARAN         
%token COMMA
%token ARROW
%token ANTISLASH
%token DOUBLE_ARROW      
%token SEMICOLON       
%token L_SQUARE_BRACKET
%token R_SQUARE_BRACKET
%token L_CURLY_BRACKET       
%token R_CURLY_BRACKET       
%token QUESTION_MARK
%token BAR       
%token IF
%token THEN
%token ELIF
%token ELSE
%token REF
%token AFFECT
%token EXCLAMATION
%token WHILE       
%token EXTERN
%token TYPE       
%token UNDERSCORE
%token AMPERSAND
%token B_OR     
%token B_AND     
%token GT
%token GE
%token LT
%token LE
%token EQUALS   
%token PLUS
%token MOIN
%token STAR       
%token DIV


%nonassoc fix_bar_low       
%right BAR
%nonassoc fix_bar_high       
%right AMPERSAND       
%nonassoc COLON       
%nonassoc fix_semicolon_low         
%right SEMICOLON
%right ELSE
%right ELIF          
%right DOUBLE_ARROW      
%right ARROW
%nonassoc REF
%right AFFECT        
%left B_OR
%left B_AND
%left GT,LT,GE,LE,EQUALS
%left INFIX_ID                 
%left PLUS MOIN
%left STAR DIV         
%nonassoc QUESTION_MARK    
%nonassoc EXCLAMATION      
%left LPARAN      
%left L_SQUARE_BRACKET
      
      
%start<HopixAST.t> program

%%
                     
(* inline function, helps to locate the error *)
%inline located(T): x=T { Position.with_poss $startpos $endpos x }

(* Grammare definition *)
(* start symbol *)                        
program:
       | EOF                        { [] }
       | p = definition_list; EOF;  { p  }

(* definition list *)                                      
definition_list:
       | v = located(vdefinition;) dl = definition_list;           { v::dl   }
       | v = located(vdefinition;)                                 { [v]     }
       | EXTERN; d = located(declare;) dl = definition_list;       { d::dl   }
       | EXTERN; d = located(declare;)                             { [d]     }
       | TYPE; dt = located(define_type;) dl = definition_list;    { dt::dl  }
       | TYPE; d = located(define_type;)                           { [d]     }                             

(* type definition  *)                          
define_type:
       | t1 = located(type_con;) t2 = ty_variable_list_p; t3 = ty_tail    { DefineType(t1,t2,t3) }

type_con:    
       | t = ID;    { TCon t }

ty_variable_list_p:                                                                                              
       |                                                                   { []      }
       | LPARAN; tv = located(type_var); tvl = ty_variable_list_rest_p;    { tv::tvl }

ty_variable_list_rest_p:                                                                   
       | RPARAN;                                                          { []      }
       | COMMA; tv = located(type_var;) tvl = ty_variable_list_rest_p;    { tv::tvl }                      
                      
ty_tail:
       |                                    { Abstract         }                                                                         
       | EQUALS;  tl = tdefinition_list;    { DefineSumType(tl) }

(* tdefinition *)
tdefinition_list:                                    
       | option(BAR;) t = tdefinition;                               { [t]   }
       | option(BAR;) t = tdefinition; tl = tdefinition_list_rest    { t::tl }

tdefinition_list_rest:
       | BAR; t = tdefinition;                                { [t]   }
       | BAR; t = tdefinition; tlr = tdefinition_list_rest    { t::tlr }

tdefinition:
       | c = located(constructor;)                                               { (c,[])     }
       | c = located(constructor;)  LPARAN; t1 = located(ty); t2 = ty_rest_p;    { (c,t1::t2) }
                                                              
(* for function definition (first use) *)                                                              

function_def:                                                                                             
       | tvl = ty_variable_list_sb; pl = pattern_list_p; EQUALS; e = located(expr;)          { FunctionDefinition(tvl,pl,e)  }
       | tvl = ty_variable_list_sb; pl = pattern_list_p; COLON; tv = located(ty_eq_expr;)    { FunctionDefinition(tvl,pl,tv) } 
function_define_rest:
       |                                                            { []        }
       | AND; fd = function_define; fd_r = function_define_rest;    { fd:: fd_r }

function_define:                                                                 
       | n = located(id;) fd = located(function_def;)    { (n , fd) }
                                                                                   
(* for declaration of external value *)
declare:                                                    
       | n = located(id;) COLON; t = located(ty;)    { DeclareExtern(n,t) }

(*vdefinition*)                                            
vdefinition:
       | VAL; n = located(id;) EQUALS; e = located(expr;)           { DefineValue(n,e)        } 
       | VAL; n = located(id;) COLON; tv = located(ty_eq_expr;)     { DefineValue(n,tv)       } 
       | FUN; fn = function_define; fd_r = function_define_rest;    { DefineRecFuns(fn::fd_r) }

(* used in definition of functions and in definition for a value  [type = expr] *)
ty_eq_expr:
       | t = located(ty;) EQUALS; e = located(expr;)    { TypeAnnotation(e,t) }     %prec fix_semicolon_low

(* expression *)
expr:
       | l = located(literal;)                                                                                    { Literal l                          }
       | n = located(id;)                                                                                         { Variable n                         }
       | n = located(constructor;) el = expr_list_or_empty;                                                       { Tagged(n, [] ,el)                  }
       | n = located(constructor;) L_SQUARE_BRACKET; t = located(ty); r = ty_rest_sb; el = expr_list_or_empty;    { Tagged(n,(t::r),el)                } 
       | LPARAN; e = located(expr;) COLON; t = located(ty;) RPARAN;                                               { TypeAnnotation(e,t)                } 
       | e1 = located(expr); SEMICOLON; e2 = located(expr);                                                       { Define(dummy_pos("nothing"),e1,e2) }
       | v_val = vdefinition_val; SEMICOLON; e = located(expr;)                                                   { Define(fst v_val, snd v_val, e)    }
       | v_fun = vdefinition_fun; SEMICOLON; e = located(expr;)                                                   { DefineRec(v_fun, e)                }
       | e = located(expr;) el = expr_list;                                                                       { Apply(e,[],el)                     }
       | e = located(expr;) L_SQUARE_BRACKET; t = located(ty); r = ty_rest_sb;  el = expr_list;                   { Apply(e,(t::r),el)                 }
       | ANTISLASH; fd = function_lambda;                                                                         { Fun fd                             }
       | e1 = located(expr;) bo = located(binop;) e2 = located(expr;)                                             { Apply(bo,[],[e1;e2])               }
       | e = located(expr;) QUESTION_MARK; bl = branch_list;                                                      { Case(e,bl)                         }
       | IF; e1 = located(expr;) THEN; e2 = located(expr;) eil = elif_list; e = op_else;                          { If (([e1,e2]@eil), e)              } 
       | REF; e = located(expr;)                                                                                  { Ref e                              }
       | e1 = located(expr;) AFFECT; e2 = located(expr;)                                                          { Write(e1,e2)                       }
       | EXCLAMATION; e = located(expr;)                                                                          { Read e                             }
       | WHILE; e1 = located(expr;) L_CURLY_BRACKET; e2 = located(expr;) R_CURLY_BRACKET;                         { While(e1,e2)                       }
       | LPARAN; e = expr; RPARAN;                                                                                { e                                  }

(* used in IF-THEN-ELIF-ELSE *)
op_else:
       |                             { None   }    %prec ELSE
       | ELSE; e = located(expr;)    { Some e }

elif_list:
       |                                                                        { []           }    %prec ELIF             
       | ELIF; e1 = located(expr;) THEN; e2 = located(expr;) eil = elif_list    { (e1,e2)::eil }

(* used in list type with square bracket *)                                                                                
ty_rest_sb:
       | COMMA; t = located(ty;) r = ty_rest_sb;    { t::r }
       | R_SQUARE_BRACKET;                          { []   }

(* list expression with parenthesis *)                                                      
expr_list_or_empty:
       |                                                { []   }    %prec ARROW
       | LPARAN; e = located(expr;) r = expr_list_rest;    { e::r }  

expr_list:
       | LPARAN; e = located(expr;) r = expr_list_rest;    { e::r }  
                                                            
expr_list_rest:
       | COMMA; e = located(expr;) r = expr_list_rest;    { e::r }
       | RPARAN                                        { []   }
                                                                                                                                                      
(* definition of value in expression *)
vdefinition_val:                                  
       | VAL; n = located(id;) EQUALS; e = located(expr;)               { (n,e) } 
       | VAL; n = located(id;) COLON; ty; EQUALS; e = located(expr;)    { (n,e) } 

(* function definition in expression *)
vdefinition_fun:
       | FUN; fn = function_define_expression; fd_r = function_define_expression_rest;    { (fn::fd_r) }

function_define_expression_rest:
       |                                                            { []        }
       | AND; fd = function_define_expression; fd_r = function_define_expression_rest;    { fd:: fd_r }

function_define_expression:                                                                 
       | n = located(id;) fd = located(function_def_expression;)    { (n , fd) }                                      
                                 
function_def_expression:                                              
       | tvl = ty_variable_list_sb; pl = pattern_list_p; EQUALS; e = located(expr;)          { FunctionDefinition(tvl,pl,e)  }
       | tvl = ty_variable_list_sb; pl = pattern_list_p; COLON; tv = located(ty_eq_expr;)    { FunctionDefinition(tvl,pl,tv) }
                                                                      
(* lambda expression *)
function_lambda:                                                                                             
       | tvl = ty_variable_list_sb; pl = pattern_list_p; DOUBLE_ARROW; e = located(expr;)               { FunctionDefinition(tvl,pl,e) }
       | tvl = ty_variable_list_sb; pl = pattern_list_p; COLON; ty; DOUBLE_ARROW; e = located(expr;)    { FunctionDefinition(tvl,pl,e) }

(* pattern list [(pattern,pattern,...]*)
pattern_list_p:
       | LPARAN; p = located(pattern;) pl = pattern_list_rest_p;    { p::pl }

pattern_list_rest_p:
       | RPARAN;                                                   { []    }
       | COMMA; p = located(pattern;) pl = pattern_list_rest_p;    { p::pl }                                                  

(* pattern *)
pattern:
       | n = located(id;)                                          { PVariable n               } 
       | n = located(constructor_without_underscore;)              { PTaggedValue (n,[])       }
       | UNDERSCORE;                                               { PWildcard                 }
       | LPARAN; p = pattern; RPARAN;                              { p                         }
       | p = located(pattern;) COLON; t = located(ty;)             { PTypeAnnotation(p,t)      }
       | l = located(literal;)                                     { PLiteral l                }
       | pr = pattern_or_list;                                     { POr pr                    }    %prec fix_bar_low
       | p1 = located(pattern) AMPERSAND; p2 = located(pattern)    { PAnd ([p1;p2])            }
       | n = located(constructor;) LPARAN; p1 = located(pattern;) p2 = pattern_list_rest_p    { PTaggedValue (n,(p1::p2)) }

(* pattern list [pattern|pattern|...]*)
pattern_or_list:
       | p2 = located(pattern) BAR; p1 = located(pattern;)    { [p2;p1] }    %prec fix_bar_high
       | r = pattern_or_list; BAR; p2 = located(pattern)      { r@[p2]  }    %prec fix_bar_high

(* type_var list with bracket [[type_var,...]] *)                                                                                   
ty_variable_list_sb:                                                                                              
       |                                                                              { []      }
       | L_SQUARE_BRACKET; tv = located(type_var); tvl = ty_variable_list_rest_sb;    { tv::tvl }

ty_variable_list_rest_sb:                                                                   
       | R_SQUARE_BRACKET;                                                 { []      }
       | COMMA; tv = located(type_var;) tvl = ty_variable_list_rest_sb;    { tv::tvl }

(* type_var *)                                                                             
type_var:
       | t = TYPE_VARIABLE;    { TId t }

(* type *)                                 
ty:
       | n = ID;                                              { TyCon( TCon n , [] )           }
       | n = ID; LPARAN; t1 = located(ty); t2 = ty_rest_p;    { TyCon( TCon n , t1::t2 )       }
       | t1 = located(ty;) ARROW; t2 = located(ty;)           { TyCon( TCon "->", t1::t2::[] ) }
       | LPARAN; t = ty; RPARAN;                              { t }
       | t = TYPE_VARIABLE;                                   { TyVar (TId t) } 
                                   
ty_rest_p:
       | COMMA; t1 = located(ty;) t2 = ty_rest_p;    { t1::t2 }
       | RPARAN                                      { []     }

(* var_id *)
id:
       | n = ID           { Id n }
       | n = PREFIX_ID    { Id n }       

(* constructor *)                          
constructor:
       | n = CONSTR_ID;     { KId n   }
       | n = UNDERSCORE;    { KId "_" }

constructor_without_underscore:
       | n = CONSTR_ID;    { KId n }

(* literals *)
literal:
       | i = INT;       { LInt i    }
       | c = CHAR;      { LChar c   }                   
       | s = STRING;    { LString s }

(* branch *)                  
branch_list:
       | option(BAR;)  b = located(branch;) bl = branch_list_rest                                      { b::bl }
       | L_CURLY_BRACKET; option(BAR;)  b = located(branch;) bl = branch_list_rest R_CURLY_BRACKET;    { b::bl }

branch_list_rest:
       |                                                     { []    }    %prec BAR
       | BAR; b = located(branch;) bl = branch_list_rest;    { b::bl }

branch:
       | p = located(pattern;) DOUBLE_ARROW; e = located(expr;)    { Branch(p,e) }           

(* binary operators*)
%inline binop:                    
       | bo = located(op;)    { Variable bo }

%inline op:
       | PLUS;           { Id "`+"  }
       | MOIN;           { Id "`-"  }
       | STAR;           { Id "`*"  }
       | DIV;            { Id "`/"  }
       | GT;             { Id "`>"  }
       | GE;             { Id "`>=" }
       | LT;             { Id "`<"  }
       | LE;             { Id "`<=" }
       | B_OR;           { Id "`||" }
       | B_AND;          { Id "`&&" }
       | EQUALS;         { Id "`="  }
       | s = INFIX_ID    { Id (String.sub s 0 (String.length s - 1)) }
