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
%token <string> TYPE_VARIABLE                 
%token VAL
%token EQUALS
%token COLON
%token LPARAN                  
%token RPARAN         
%token COMMA
%token ARROW
%token L_SQUARE_BRACKET
%token R_SQUARE_BRACKET       

%nonassoc VAL
%nonassoc COMMA
%right ARROW          
      
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
       | v = located(vdefinition;) t = definition_list  { v::t }
       | v = located(vdefinition;) { [v] }
                
vdefinition:
       | VAL; n = located(id;) EQUALS; e = located(expr;) { DefineValue(n,e) }
       | VAL; n = located(id;) COLON; ty; EQUALS; e = located(expr;) { DefineValue(n,e) }

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
       | l = located(literal;)                                                 { Literal l }
       | n = located(id;)                                                      { Variable n }
       | n = located(constructor;) tl = type_list; el = expr_list;             { Tagged(n,tl,el) }

type_list:
       |                                                                      { [] }
       | L_SQUARE_BRACKET; t = located(ty); r = ty_rest_sb;                   { t::r }

ty_rest_sb:
       | COMMA; t = located(ty;) r = ty_rest_sb;                 { t::r }
       | R_SQUARE_BRACKET;                                       { [] }
                                                                                     
expr_list:
       |                                                  { [] }
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
