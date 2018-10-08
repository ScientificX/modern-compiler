module TypeError (TypeError(..)) where

import qualified Abs
import qualified Types

-- Instead of just calling `error` we will make use of error monads because Haskell is dope
data TypeError = UndefinedVariable Abs.Pos String
               | NotAFunction Abs.Pos String
               | UndefinedFunction Abs.Pos String
               | ArgumentCountMismatch Abs.Pos String Int Int -- _ pos name expected given
               | TypeNotFound Abs.Pos String
               | RecordTypeNotFound Abs.Pos Types.Ty
               | IfExpectsTruth Abs.Pos Types.Ty
               | TypeMismatch Abs.Pos Types.Ty Types.Ty -- _ pos expected given
               | UnknownArrayType Abs.Pos Types.Ty
               | FieldVarRequiresRecord Abs.Pos Types.Ty
               | FieldNotFound Abs.Pos String
               | SubscriptVarRequiresArray Abs.Pos Types.Ty
               | InvalidNil Abs.Pos
               | DuplicateFunctionParameters Abs.Pos
               | UnknownFunctionParamterType Abs.Pos
               | FunctionTypeInvalid Types.Ty Types.Ty Abs.Pos
               | UndefinedType String Abs.Pos
               | FunctionTypeNotFound Abs.Pos
               | UnknownError String
               | InvalidOp Types.Ty Types.Ty String Abs.Pos

-- Because we made an error monad we can do neato error printing
showPos :: Abs.Pos -> String
showPos (Abs.Pos l c) = "Line: " ++ show l ++ " | Column: " ++ show c

showError :: Abs.Pos -> String -> String
showError pos s = s ++ "\n" ++ showPos pos

instance Show TypeError where
  show (UndefinedVariable pos name) =
    showError pos $ "Undefined variable: " ++ name
  show (NotAFunction pos name) =
    showError pos $ "Not a function: " ++ name
  show (UndefinedFunction pos name) =
    showError pos $ "Undefined function: " ++ name
  show (ArgumentCountMismatch pos name expected given) =
    showError pos $ "Wrong number of arguments for '" ++ name ++ "'\nExpected: " ++ show given ++ "\nGiven: " ++ show given
  show (TypeNotFound pos t) =
    showError pos $ "Type not found: " ++ t
  show (RecordTypeNotFound pos t) =
    showError pos $ "Expected Record type got: " ++ show t
  show (IfExpectsTruth pos given) =
    showError pos $ "If expression tested invalid type: " ++ show given
  show (TypeMismatch pos expected given) =
    showError pos $ "Type mismatch. \nExpected: " ++ show expected ++ "\nGiven: " ++ show given
  show (UnknownArrayType pos t) =
    showError pos $ "Unknown type for Array: " ++ show t
  show (FieldVarRequiresRecord pos t) =
    showError pos $ "FieldVar requires Record type.\nGiven: " ++ show t
  show (FieldNotFound pos n) =
    showError pos $ "Field not found: " ++ n
  show (SubscriptVarRequiresArray pos t) =
    showError pos $ "SubscriptVar requires Array type.\nGiven: " ++ show t
  show (InvalidNil pos) =
    showError pos $ "Unexpected Nil"
  show (DuplicateFunctionParameters pos) =
    showError pos $ "Duplicate parameters in function definition"
  show (UnknownFunctionParamterType pos) =
    showError pos $ "Unknown function parameter type"
  show (FunctionTypeInvalid fnTy resultTy pos) =
    showError pos $ "Function declared as " ++ show fnTy ++ " but returned " ++ show resultTy
  show (UndefinedType ty pos) =
    showError pos $ "Undefined type: " ++ ty
  show (FunctionTypeNotFound pos) =
    showError pos $ "Function type not found."
  show (UnknownError s) =
    "Unknown error happened.\n" ++ s
  show (InvalidOp l r o pos) =
    showError pos $ "Invalid Op expression. \nLeft: " ++ show l ++ "\nRight: " ++ show r ++ "\nOpType: " ++ show o
