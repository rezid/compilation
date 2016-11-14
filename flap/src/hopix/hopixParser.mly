%{
    open HopixAST
%}
   
%token EOF
%token<Int32.t> INT
%token<string> VAR_ID                  
%token VAL
%token EQUALS               
                  
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
       | VAL; n = located(id); EQUALS; e = located(expr); 
          {DefineValue(n,e)}

id:
       | n = VAR_ID {Id n}

expr:
       | l = located(literal;) { Literal l }

literal:
       | i = INT; { LInt i }
            
