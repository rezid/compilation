%{
    open HopixAST
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
%right AFFECT
%nonassoc QUESTION_MARK
%left B_OR
%left B_AND
%left GT,LT,GE,LE,EQUALS
%left PLUS MOIN
%left STAR DIV
%left INFIX_ID      
%left LPARAN      
%left L_SQUARE_BRACKET
%nonassoc REF
%nonassoc EXCLAMATION


          
      
%start<HopixAST.t> program

%%
(*   inline functions *)
%inline located(T): x=T { Position.with_poss $startpos $endpos x }

(* Grammare definition *)
program:
       | EOF { [] }
       | e = definition_list; EOF; { e }
       ;
   
definition_list:
       | v = located(vdefinition;) d = definition_list;                                { v::d }
       | v = located(vdefinition;)                                                     { [v] }
       | EXTERN; e = located(declare;) d = definition_list;                            { e::d }
       | EXTERN; e = located(declare;)                                                 { [e] }
       | TYPE; dt = located(define_type;) dl = definition_list;                        { dt::dl }
       | TYPE; d = located(define_type;)                                               { [d] }                             

define_type:
       | t1 = located(ty_con;) t2 = ty_variable_list_p; t3 = ty_d                      { DefineType(t1,t2,t3) }

ty_d:
       |                                    { Abstract }                                                                         
       | EQUALS;  l = first_d;              { DefineSumType(l) }

first_d:                                    
       | option(BAR;) t = tdefinition;                          { [t] }
       | option(BAR;) t = tdefinition; dl = d_list_rest         { t::dl }

d_list_rest:
       | BAR; t = tdefinition;                          { [t] }
       | BAR; t = tdefinition; dl = d_list_rest         { t::dl }
                            
tdefinition:
       | c = located(constructor;)                                             { (c,[]) }
       | c = located(constructor;)  LPARAN; t1 = located(ty); t2 = ty_rest_p;  { (c,t1::t2) }

ty_con:    
       | t = ID;  { TCon t }
  
ty_variable_list_p:                                                                                              
       |                                                                              { [] }
       | LPARAN; tv = located(type_var); tvl = ty_variable_list_rest_p;               { tv::tvl }

ty_variable_list_rest_p:                                                                   
       | RPARAN;                                                                     { [] }
       | COMMA; tv = located(type_var;) tvl = ty_variable_list_rest_p;               { tv::tvl }


         
declare:                                                    
       | n = located(id;) COLON; t = located(ty;)                          { DeclareExtern(n,t) }

vdefinition:
       | VAL; n = located(id;) EQUALS; e = located(expr;)             { DefineValue(n,e) } 
       | VAL; n = located(id;) COLON; tv = located(ty_vdef;)          { DefineValue(n,tv) } 
       | FUN; fn = function_define_v1; fd_r = function_define_rest_v1;      { DefineRecFuns(fn::fd_r) }

ty_vdef:
       | t = located(ty;) EQUALS; e = located(expr;)  { TypeAnnotation(e,t) } %prec fix_semicolon_low


vdefinition_val:                                  
       | VAL; n = located(id;) EQUALS; e = located(expr;)                { (n,e) } 
       | VAL; n = located(id;) COLON; ty; EQUALS; e = located(expr;)     { (n,e) } 

vdefinition_fun:
       | FUN; fn = function_define; fd_r = function_define_rest;         { (fn::fd_r) }

function_define_rest:
       |                                                                              { [] }
       | AND; fd = function_define; fd_r = function_define_rest;                      { fd:: fd_r }

function_define_rest_v1:
       |                                                                              { [] }
       | AND; fd = function_define_v1; fd_r = function_define_rest_v1;                      { fd:: fd_r }
                                                                                        
function_define:                                                                 
       | n = located(id;) fd = located(function_def;)                                 { (n , fd) }

function_define_v1:                                                                 
       | n = located(id;) fd = located(function_def_v1;)                                 { (n , fd) }
                                      
function_def_v1:                                                                                             
       | tvl = ty_variable_list_sb; pl = pattern_list_p; EQUALS; e = located(expr;)              { FunctionDefinition(tvl,pl,e) } 
       | tvl = ty_variable_list_sb; pl = pattern_list_p; COLON; tv = located(ty_vdef;)           { FunctionDefinition(tvl,pl,tv) }
                                      
function_def:                                                                                             
       | tvl = ty_variable_list_sb; pl = pattern_list_p; EQUALS; e = located(expr;)              { FunctionDefinition(tvl,pl,e) }
       | tvl = ty_variable_list_sb; pl = pattern_list_p; COLON; tv = located(ty_vdef;)           { FunctionDefinition(tvl,pl,tv) }

function_def_arrow:                                                                                             
       | tvl = ty_variable_list_sb; pl = pattern_list_p; DOUBLE_ARROW; e = located(expr;)              { FunctionDefinition(tvl,pl,e) }
       | tvl = ty_variable_list_sb; pl = pattern_list_p; COLON; ty; DOUBLE_ARROW; e = located(expr;)   { FunctionDefinition(tvl,pl,e) }
                                                                                       
                                                                                       
pattern_list_p:
       | LPARAN; p = located(pattern;) pl = pattern_list_rest_p;                      { p::pl }

pattern_list_rest_p:
       | RPARAN;                                                                      { [] }
       | COMMA; p = located(pattern;) pl = pattern_list_rest_p;                       { p::pl }                                                  

ty_variable_list_sb:                                                                                              
       |                                                                              { [] }
       | L_SQUARE_BRACKET; tv = located(type_var); tvl = ty_variable_list_rest_sb;    { tv::tvl }

ty_variable_list_rest_sb:                                                                   
       | R_SQUARE_BRACKET;                                                            { [] }
       | COMMA; tv = located(type_var;) tvl = ty_variable_list_rest_sb;               { tv::tvl }

type_var:
       | t = TYPE_VARIABLE;  { TId t }

ty:
       | n = ID;                                                 { TyCon( TCon n , [] ) }
       | n = ID; LPARAN; t1 = located(ty); t2 = ty_rest_p;       { TyCon( TCon n , t1::t2 ) }
       | t1 = located(ty;) ARROW; t2 = located(ty;)              { TyCon( TCon "->", t1::t2::[] ) }
       | LPARAN; t = ty; RPARAN;                                 { t }
       | t = TYPE_VARIABLE;                                      { TyVar (TId t) } 
                                   
ty_rest_p:
       | COMMA; t1 = located(ty;) t2 = ty_rest_p;                { t1::t2 }
       | RPARAN                                                  { [] }

id:
       | n = ID {Id n}
       | n = PREFIX_ID {Id n}       
                
expr:
       | l = located(literal;)                                                                                    { Literal l }
       | n = located(id;)                                                                                         { Variable n }
       | n = located(constructor;) el = expr_list;                                                                { Tagged(n, [] ,el) }
       | n = located(constructor;) L_SQUARE_BRACKET; t = located(ty); r = ty_rest_sb; el = expr_list;             { Tagged(n,(t::r),el) }            
       | LPARAN; e = located(expr;) COLON; t = located(ty;) RPARAN;                                               { TypeAnnotation(e,t) } 
       | e1 = located(expr); SEMICOLON; e2 = located(expr);                            { Define((Position.with_poss Lexing.dummy_pos Lexing.dummy_pos (Id "nothing")),e1,e2) }
       | v_val = vdefinition_val; SEMICOLON; e = located(expr;)                                                   { Define(fst v_val, snd v_val, e) }
       | v_fun = vdefinition_fun; SEMICOLON; e = located(expr;)                                                   { DefineRec(v_fun, e) }
       | e = located(expr;) el = expr_lst;                                                                        { Apply(e,[],el) }
       | e = located(expr;) L_SQUARE_BRACKET; t = located(ty); r = ty_rest_sb;  el = expr_lst;                    { Apply(e,(t::r),el) }
       | ANTISLASH; fd = function_def_arrow;                                                                      { Fun fd }
       | e1 = located(expr;) bo = located(binop;) e2 = located(expr;)                                             { Apply(bo,[],[e1;e2]) }     
       | e = located(expr;) QUESTION_MARK; bl = branch_list;                                                      { Case(e,bl) }
       | IF; e1 = located(expr;) THEN; e2 = located(expr;) eil = elif_list; e = op_else;                          { If (([e1,e2]@eil), e) } 
       | REF; e = located(expr;)                                                                                  { Ref e }
       | e1 = located(expr;) AFFECT; e2 = located(expr;)                                                          { Write(e1,e2) }
       | EXCLAMATION; e = located(expr;)                                                                          { Read e }
       | WHILE; e1 = located(expr;) L_CURLY_BRACKET; e2 = located(expr;) R_CURLY_BRACKET;                         { While(e1,e2) }
       | LPARAN; e = expr; RPARAN;                                                                                { e }

                                                                                                                    
op_else:
       |                           { None }    %prec ELSE                                                                                                                
       | ELSE; e = located(expr;)  { Some e }

elif_list:
       |                                                                             { [] }     %prec ELIF             
       | ELIF; e1 = located(expr;) THEN; e2 = located(expr;) eil = elif_list;        { (e1,e2)::eil }

ty_rest_sb:
       | COMMA; t = located(ty;) r = ty_rest_sb;                 { t::r }
       | R_SQUARE_BRACKET;                                       { [] }
                                                                                     
expr_list:
       |                                                  { [] } %prec ARROW
       | LPARAN; e = located(expr;) r = expr_rest_p;      { e::r }  

expr_lst:
       | LPARAN; e = located(expr;) r = expr_rest_p;      { e::r }  

                                                            
expr_rest_p:
       | COMMA; e = located(expr;) r = expr_rest_p;              { e::r }
       | RPARAN                                                  { [] }
                                                            
constructor:
       | n = CONSTR_ID;   { KId n }
       | n = UNDERSCORE;  { KId "_" }

constructor_without_underscore:
       | n = CONSTR_ID;   { KId n }
                                                                                 
literal:
       | i = INT; { LInt i }
       | c = CHAR; { LChar c }                   
       | s = STRING; { LString s }

pattern:
       | n = located(id;)                                                                                   { PVariable n }
       | n = located(constructor_without_underscore;)                                                       { PTaggedValue (n,[]) }
       | UNDERSCORE;                                                                                        { PWildcard }
       | LPARAN; p = pattern; RPARAN;                                                                       { p }
       | p = located(pattern;) COLON; t = located(ty;)                                                      { PTypeAnnotation(p,t) }
       | l = located(literal;)                                                                              { PLiteral l }
       | pr = x                                                                                             { POr pr } %prec fix_bar_low
       | p1 = located(pattern) AMPERSAND; p2 = located(pattern)                                             { PAnd ([p1;p2]) }                                                
       | n = located(constructor;) LPARAN; p1 = located(pattern;) p2 = pattern_lst                          { PTaggedValue (n,(p1::p2)) }


x:
       | p2 = located(pattern) BAR; p1 = located(pattern;)             { [p2;p1]    } %prec fix_bar_high
       | r = x; BAR; p2 = located(pattern)                             { r@[p2]     } %prec fix_bar_high
                                                                                                            
pattern_lst:
       | RPARAN;                                              { [] }
       | COMMA; p = located(pattern;) pl = pattern_lst;       { p::pl }

%inline binop:                    
       | bo = located(op;)  { Variable bo }

%inline op:
       | PLUS;         { Id "`+" }
       | MOIN;         { Id "`-" }
       | STAR;         { Id "`*" }
       | DIV;          { Id "`/" }
       | GT;           { Id "`>" }
       | GE;           { Id "`>=" }
       | LT;           { Id "`<" }
       | LE;           { Id "`<=" }
       | B_OR;         { Id "`||" }
       | B_AND;        { Id "`&&" }
       | EQUALS;       { Id "`=" }
       | s = INFIX_ID  { Id (String.sub s 0 (String.length s - 1)) }
                  
branch_list:
       | option(BAR;)  b = located(branch;) bl = branch_list_rest    { b::bl }
       | L_CURLY_BRACKET; option(BAR;)  b = located(branch;) bl = branch_list_rest R_CURLY_BRACKET;   { b::bl }

branch_list_rest:
       |                                                    { [] }      %prec BAR                                                                              
       | BAR; b = located(branch;) bl = branch_list_rest;  { b::bl }

branch:
       | p = located(pattern;) DOUBLE_ARROW; e = located(expr;)    { Branch(p,e) }           
