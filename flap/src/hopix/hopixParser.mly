%{
    open HopixAST           
%}
   
%token EOF
%token<Int32.t> INT
%token<char> CHAR                  
%token<string> ID
%token<string> STRING                              
%token<string> PREFIX_ID
%token<string> CONSTR_ID                                 
%token<string> TYPE_VARIABLE                 
%token VAL
%token FUN                      
%token AND
%token EQUALS
%token COLON
%token LPARAN                  
%token RPARAN         
%token COMMA
%token ARROW
%token SEMICOLON       
%token L_SQUARE_BRACKET
%token R_SQUARE_BRACKET       

%left SEMICOLON
%right ARROW       
%left EQUALS
%left LPARAN      
%left L_SQUARE_BRACKET
      
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
       | v = located(vdefinition;) t = definition_list;    { v::t }
       | v = located(vdefinition;)                         { [v] }
                    
vdefinition:
       | VAL; n = located(id;) EQUALS; e = located(expr;)  { DefineValue(n,e) } 
       | VAL; n = located(id;) COLON; ty; EQUALS; e = located(expr;)  { DefineValue(n,e) }
       | FUN; fn = function_define; fd_r = function_define_rest;  { DefineRecFuns(fn::fd_r) }

vdefinition_val:                                  
       | VAL; n = located(id;) EQUALS; e = located(expr;)                { (n,e) } 
       | VAL; n = located(id;) COLON; ty; EQUALS; e = located(expr;)     { (n,e) } 

vdefinition_fun:
       | FUN; fn = function_define; fd_r = function_define_rest;         { (fn::fd_r) }

function_define_rest:
       |                                                                              { [] }
       | AND; fd = function_define; fd_r = function_define_rest;                      { fd:: fd_r }

function_define:                                                                 
       | n = located(id;) fd = located(function_def;)                                 { (n , fd) }
                                      
function_def:                                                                                             
       | tvl = ty_variable_list_sb; pl = pattern_list_p; EQUALS; e = located(expr;)              { FunctionDefinition(tvl,pl,e) }
       | tvl = ty_variable_list_sb; pl = pattern_list_p; COLON; ty; EQUALS; e = located(expr;)   { FunctionDefinition(tvl,pl,e) }
                                                                            
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
       | t1 = located(ty;) ARROW; t2 = located(ty;)              { TyCon( TCon "", [t1]@[t2] ) }
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
       | e1 = expr; SEMICOLON; e2 = expr;                                                                         { e2 }
       | v_val = vdefinition_val; SEMICOLON; e = located(expr;)                                                   { Define(fst v_val, snd v_val, e) }
       | v_fun = vdefinition_fun; SEMICOLON; e = located(expr;)                                                   { DefineRec(v_fun, e) }
       | e = located(expr;) el = expr_lst;                                                                        { Apply(e,[],el) }
       | e = located(expr;) L_SQUARE_BRACKET; t = located(ty); r = ty_rest_sb;  el = expr_lst;                    { Apply(e,(t::r),el) }
                                                                                                                    

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
                                                                                 
literal:
       | i = INT; { LInt i }
       | c = CHAR; { LChar c }                   
       | s = STRING; { LString s }

pattern:
       | n = located(id;) { PVariable n }
