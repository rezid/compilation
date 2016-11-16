%{
    open HopixAST
%}
   
%token EOF
%token<Int32.t> INT
%token<char> CHAR                  
%token<string> ID                  
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
       | v = located(vdefinition;) {[v]}
                
vdefinition:
       | VAL; n = located(id;) EQUALS; e = located(expr;) { DefineValue(n,e) }
       | VAL; n = located(id;) COLON; type_1; EQUALS; e = located(expr;) { DefineValue(n,e) }

type_1:
       | ID; {}
       | ID; LPARAN; type_1; type_2; {}
       | type_1; ARROW; type_1 {}
       | LPARAN; type_1; RPARAN; {}
       | TYPE_VARIABLE; {}
                                   
type_2:
       | COMMA; type_1; type_2 {}
       | RPARAN {}

id:
       | n = ID {Id n}
       | n = PREFIX_ID {Id n}       
                
expr:
       | l = located(literal;) { Literal l }                    

literal:
       | i = INT; { LInt i }
       | c = CHAR; { LChar c }                   
            
