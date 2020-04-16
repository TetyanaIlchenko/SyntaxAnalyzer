   MiniJava
   Потрібний синтаксис зручний для синтаксичного аналізу
   .. граматика повинна бути однозначною, враховувати пріоритети операцій
   Використовується ітераційна форма граматик ( |, [, ], {, } - метасимволи) 
 
program     =  mainClass { classDecl }
mainClass   =  "class" id 
                    "{" "public" "static" "void" "main" "(" "String" "[""]" id ")"
                             "{"  statement "}" 
				    "}" 
classDecl   =  "class" id ["extends" id ]
                    "{"  {varDecl} {methodDecl} "}"
varDecl     =  type id ";"	
methodDecl  =  "public" type id "(" [formalList] ")"
                     "{" {varDecl} {statement} "return" exp ";" "}"			
formalList  =  type id {"," type id }		
type      	=  "int" ["[" "]"] | "boolean" | id

statement   = "{" {statement} "}" | "if" "(" exp ")" statement "else" statement	
            | "while" "(" exp ")" statement  | "System.out.println" "(" exp ")" ";" 
            | id ["[" exp "]"] = exp ";"	

exp 	    = full {"&&" full}
full        = simple ["<" simple]
simple      = term {addOp term}
term        = fact { "*" fact}
fact        = { "!"}  access
access      = base 
               {"[" exp "]"  | "." "length" | "." id "(" [ factList] ")"}
base        = "(" expr ")" | number | id | "true" | "false" | "this" | 
            | "new" id "(" ")" | "new" "int" "[" exp "]" 
factList    = exp { "," exp} 				
            
		