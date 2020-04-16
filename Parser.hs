{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Attoparsec.Text
import Data.ParserCombinators.Attoparsec.Char8
import Types
import Data.Char
import Data.Attoparsec.Expr

file = ""

insidebraces = (parseStringWithSpaces "{") manyTill anyChar (parseStringWithSpaces "}")
insideRoundBraces = (parseStringWithSpaces "(") manyTill anyChar (parseStringWithSpaces ")")
insideSquareBraces = (parseStringWithSpaces "[") manyTill anyChar (parseStringWithSpaces "]")

 main::IO()
 main = do
 	input <- readFile file
 	print $ parse programParse input


parseStringWithSpaces:: String -> Parser String
parseStringWithSpaces str = do {skipMany space; string str; skipMany space; return str} -- розпарсили стрічку з пробілами і повернули її
                            
				   		    

parseDigits:: Parser Char
parseDigits = satisfy isDigit
          where isDigit d = d >= '1' && d <= '9'  -- повертає якийсь розпарсений символ з перелічених

digitsIntoLitterals:: Parser Integer
digitsIntoLitterals = try (do {l <- parseDigits; lt <- many' digits; return (read (l:lt)::Integer})
                  <|> do {_ <- char '0'; return 0} -- повертає літерал символа
		 
--tabOrNL :: Char -> Bool
--tabOrNL x = x == '\t' || x == '\n'
		 

programParse:: Parser Program1
programParse = do { mainCl <- mainClassDeclaration; skipMany space; classDcl <- many' classDeclaration; return (Program1 mainCl classDcl)} 

mainClassDeclaration:: Parser Decl
mainClassDeclaration = do { parseStringWithSpaces "public"; mainClassId <- getClassId; skipMany space; mainClBody <- insidebraces mainClassBody; return (MainClassDeclaration mainClassId mainClBody)}

classDeclaration:: Parser Decl
classDeclaration = do
                   parseStringWithSpaces "class"
                   classId <- getClassId
				   skipMany space
				   extendClass <- do{ parseStringWithSpaces "extends"; extendId <- getClassId; return [extendId]}
				   clBody <- insidebraces classBody
				   return (ClassDeclaration classId extendClass clBody)}
		--		   where 
		--		   	 extendDecl = do {parseStringWithSpaces "extends"; extendId <- getClassId; return [extendId]}

mainClassBody:: Parser Body
mainClassBody = do 
                parseStringWithSpaces "public"
                parseStringWithSpaces "static"
	      		parseStringWithSpaces "void"
				parseStringWithSpaces "main"
				parameters <- parseParams
				skipmany space
				statement <- insidebraces (many' parseStatement)
				return (MainClassBody parameters statement)}
				where
					parseParams = do
						          parseStringWithSpaces "(";
						          parseStringWithSpaces "String";
						          insideSquareBraces;
						          id <- getClassId;
						          parseStringWithSpaces ")";
						          return id
					

classBody:: Parser Body -- try?
classBody = do { varList <- many' (try(getVarDeclList)) ; skipMany space; methodList <- many' (try(getMethodDeclList)); return (ClassBody varList methodList)}

getClassId:: Parser Exp
getClassId = do {s <- letter; s1 <- many' (digit <|> letter <|> char '_'); return (s:s1)}

getVarDeclList:: Parser Declaration
getVarDeclList = do { varType <- getType; skipMany space; varId <- getClassId; skipMany space; parseStringWithSpaces ";"; return (VarDeclaration varType varId)}

getType:: Parser Types
getType = try (do { parseStringWithSpaces "boolean"; return Boolean}) 
       <|> try (do { parseStringWithSpaces "int"; return Int})
       <|> try (do { parseStringWithSpaces "int"; insideSquareBraces; return Int}) 
       <|> try(do { id <- getClassId; return (Reference id)})

getMethodDeclList:: Parser Declaration
getMethodDeclList = do { parseStringWithSpaces "public"; methodType <- getType; skipMany space; methodId <- getClassId; skipMany space; formalList <- insideRoundBraces parseFormalList; skipMany space; methodBody <- insidebraces parseMethodBody; return (MethodDeclaration methodType methodId formalList methodBody)}


parseMethodBody:: Parser Body
parseMethodBody = do
                  varList <- many' (try(getVarDeclList))
				  statementList <- many' (try(parseStatement))
				  returnExp <- rExp 
                  return (MethodBody varList statementList returnExp)
                  where rExp = do 
                  	           parseStringWithSpaces "return"
                  	           expression <- getExp
                  	           skipMany spaces
                  	           parseStringWithSpaces ";"
                  	           skipMany spaces
                  	           return expression
				  
parseFormalVar:: Parser FormalVar
parseFormalVar = do {varType <- getType; varId <- getClassId; return (varType, varId)}
	             
	             

parseFormalList:: Parser  FormalList
parseFormalList = sepBy parseFormalVar (string ",");

parseStatement:: Parser Statement
parseStatement = try ( do {parseStringWithSpaces "if"; ifElseExp <- insideRoundBraces getExp; skipMany space; statement1 <- parseStatement; parseStringWithSpaces "else"; skipMany space; statement2 <- parseStatement; return (IfElse ifElseExp statement1 statement2)})
              <|> try ( do {parseStringWithSpaces "while"; whileExp <- insideRoundBraces getExp; skipMany space; st <- parseStatement; return (While whileExp st)})
			  <|> try ( do {parseStringWithSpaces "System.out.println"; systExp <- insideRoundBraces getExp; skipMany space; string ";"; skipMany space; return (SystemPrntLn systExp)})
			  <|> try ( do {arrayId <- getClassId; skipMany space; arrayExp1 <- insideSquareBraces getExp; parseStringWithSpaces "="; arrayExp2 <- getExp; skipMany space; string ";"; skipMany space; return (ArrayEquality arrayId arrayExp1 arrayExp2)})
			  <|> try ( do {id <- getClassId; skipMany space; parseStringWithSpaces "="; idExp <- getExp; skipMany space; string ";"; skipMany space; return (Equality id idExp)})
		      <|> try ( do {s <- insidebraces (many' parseStatement); return (StatementList s)})

getExp:: Parser Exp
getExp = buildExpressionParser ops mainExp
		   
mainExp:: Parser Exp
mainExp = try(createArray)
       <|> try(createIdExp)
       <|> try(createObject) 
       <|> try(thisExp) 
       <|> try(falseExp) 
       <|> try(trueExp) 
       <|> try (callMethod)
       <|> try (arrayLength)
       <|> try (arrayLp) 
       <|> try(number) 
       <|> try(notOp) 
       <|> getClassId

createArray:: Parser Exp
createArray = do { parseStringWithSpaces "new"; typeArray <- getType; expr <- insideSquareBraces getExp; return (CreateArray typeArray expr)}

createIdExp:: Parser Exp
createIdExp = do { id <- getClassId; skipMany space; return (IdExp id)}

createObject:: Parser Exp
createObject = do { parseStringWithSpaces "new"; idObject <- getClassId; skipMany space; string "("; skipMany space; string ")"; skipmany space; return (CreateObject (Reference id)}

thisExp:: Parser Exp
thisExp = do { parseStringWithSpaces "this"; return ThisExp}


falseExp:: Parser Exp
falseExp = do { parseStringWithSpaces "false"; return FalseExp}

trueExp:: Parser Exp
falseExp = do { parseStringWithSpaces "true"; return TrueExp}

callMethod:: Parser Exp
callMethod = do e1 <- expHelper
                parseStringWithSpaces "."
                id <- getClassId
                e2 <- insideRoundBraces getExprList
                ex <- expression1 (CallMethod e1 id e2)
                return (ex)
                where 
                 getExprList::Parser [Exp]
                 getExprList = sepBy getExp (parseStringWithSpaces ",")

arrayLength:: Parser Exp
arrayLength = do { exp <- expHelper; parseStringWithSpaces ".length"; ex <- expression1(ArrayLength exp); return (ex)}

arrayLp:: Parser Exp
arrayLp = do { e1 <- expHelper; e2 <- insideSquareBraces getExp; ex <- expression1 (ArrayLp e1 e2); return (ex)}

number:: Parser Exp
number = do  
         skipMany space
         numberExp <- digitsIntoLitterals
         skipMany space
         return (Number numberExp)

notOp:: Parser Exp
notOp = do { parseStringWithSpaces "!"; skipMany space; exp <- mainExp; skipMany space; return (NotOperator exp)}

expHelper:: Parser Exp
expHelper = try(createArray)
         <|> try(createObject)
         <|> try(thisExp)
         <|> try(falseExp)
         <|> try(trueExp)
         <|> try(number)
         <|> try(notOp)
         <|> getClassId


expression1:: Exp -> Parser Exp
expression1 expr =  try (do { e2 <- insideSquareBraces getExp; res <- expression1 (ArrayLp expr e2); return (res) })
                <|> try (do { parseStringWithSpaces ".length"; res <- expression1 (ArrayLength expr); return (res) })
                <|> try (do { parseStringWithSpaces "."; id <-getClassId; e <- insideRoundBraces getExprList; res <- expression1 (CallMethod expr id e); return (res)})
                <|> do {return expr}
                 where 
                 getExprList::Parser [Exp]
                 getExprList = sepBy getExp (parseStringWithSpaces ","); 
                 

ops = [ [ Infix (reservedOp "*" >>  return (Op Mult)) AssocLeft],
        [ Infix (reservedOp "-" >>  return (Op Minus)) AssocLeft,
		  Infix (reservedOp "+" >>  return (Op Plus)) AssocLeft],
		[ Infix (reservedOp ">" >>  return (Op Gt)) AssocNone,
		  Infix (reservedOp "<" >>  return (Op Lt)) AssocNone],
		[ Infix (reservedOp "&&" >> return (Op And)) AssocLeft]
      ]		