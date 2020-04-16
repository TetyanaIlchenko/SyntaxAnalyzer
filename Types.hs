module Types where


type Id = String
type FormalList = [FormalVar]
type FormalVar = (Type,Id)

data Type = Boolean
          | Int
		  | IntArray
		  | Reference Id
		  deriving (Show)
		  
data Program1 = Program Decl [Decl] deriving (Show)

data Body = MainClassBody Id [Statement]
          | ClassBody [Decl] [Decl]
		  | MethodBody [Decl] [Statement] Exp
		  deriving (Show)

data Decl = MainClassDeclaration Id Body
          | ClassDeclaration Id [ExtendsId] Body
		  | MethodDeclaration Type Id FormalList Body
		  | VarDeclaration Type Id
		  deriving (Show)

data Statement = StatementList [Statement]
               | IfElse Statement Exp
			   | While Statement Exp
			   | SystemPrntLn Exp
			   | Equality Id Exp 
			   | ArrayEquality Id Exp Exp
			   deriving (Show)

data Exp = OpExp Op Exp Exp
         | ArrayLp Exp Exp
		 | ArrayLength Exp
         | CallMethod Exp Id [Exp]
         | TrueExp
         | FalseExp
         | ThisExp
		 | Number Exp
         | CreateObject Type
         | IdExp Id
		 | NotOperator Exp
         | CreateArray Type Exp
         deriving(Show)

data Op = And | Plus | Minus| Mult | Lt | Gt deriving (Show)
			   
reservedOp = ["+", "-", "*", ">", "<", "!", "="]
