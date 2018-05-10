module Abs where

type Symbol = String

data Pos = Pos !Int !Int deriving (Show)

data Var = SimpleVar Symbol Pos
         | FieldVar Var Symbol Pos
         | SubscriptVar Var Exp Pos
         deriving (Show)

data Exp = VarExp Var
         | NilExp
         | IntExp Integer
         | StringExp String Pos
         | CallExp Symbol [Exp] Pos
         | OpExp Exp Oper Exp Pos
         | RecordExp [(Symbol, Exp, Pos)] Symbol Pos
         | SeqExp [Exp]
         | AssignExp Var Exp Pos
         | IfExp Exp Exp (Maybe Exp) Pos
         | WhileExp Exp Exp Pos
         | ForExp Symbol Bool Exp Exp Exp Pos
         | BreakExp Pos
         | LetExp [Dec] Exp Pos
         | ArrayExp Symbol Exp Exp Pos
         deriving(Show)

data Dec = FunctionDec [FuncDec]
         | VarDec Symbol Bool (Maybe Symbol) Exp Pos
         | TypeDec [(Symbol, Ty, Pos)]
         deriving (Show)

data Ty = NameTy Symbol Pos
        | RecordTy [Field]
        | ArrayTy Symbol Pos
        deriving (Show)

data Oper = PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp deriving (Show)

data Field = Field Symbol Bool Symbol Pos deriving (Show)

data FuncDec = FuncDec Symbol [Field] (Maybe Symbol) Exp Pos deriving (Show)
