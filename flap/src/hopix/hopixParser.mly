%{
    open HopixAST
%}
   
%token EOF
%token<Int32.t> INT
%token<char> CHAR                  
%token<string> ID
%token<string> STRING                              
%token <string> PREFIX_ID
%token <string> TYPE_VARIABLE                 
%token VAL
%token EQUALS
%token COLON
%token LPARAN                  
%token RPARAN         
%token COMMA
%token ARROW       

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
       | n = ID;                                               { TyCon( TCon n , [] ) }
       | n = ID; LPARAN; t1 = located(ty); t2 = ty_rest;       { TyCon( TCon n , t1::t2 ) }
       | t1 = located(ty;) ARROW; t2 = located(ty;)            { TyCon( TCon "", [t1]@[t2] ) }
       | LPARAN; t = ty; RPARAN;                               { t }
       | t = TYPE_VARIABLE;                                    { TyVar (TId t) } 
                                   
ty_rest:
       | COMMA; t1 = located(ty;) t2 = ty_rest;                { t1::t2 }
       | RPARAN                                                

id:
       | n = ID {Id n}
       | n = PREFIX_ID {Id n}       
                
expr:
       | l = located(literal;) { Literal l }
       | n = located(id;) { Variable n }

literal:
       | i = INT; { LInt i }
       | c = CHAR; { LChar c }                   
       | s = STRING; { LString s }
