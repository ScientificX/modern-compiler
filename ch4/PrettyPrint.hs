module PrettyPrint (prettyExp) where

import Abs

import Data.List (intercalate)

prettyExp :: Exp -> String
prettyExp NilExp = " nil "
prettyExp (IntExp i) = show i
prettyExp (StringExp s _) = "\"" ++ s ++ "\""
prettyExp (BreakExp _) = " break "
prettyExp (OpExp e1 op e2 _) = (prettyExp e1) ++ " " ++ (prettyOp op) ++ " " ++ (prettyExp e1)
prettyExp (CallExp s exps _) = s ++ "(" ++ (intercalate ", " $ map prettyExp exps) ++ ")"
prettyExp (SeqExp exps) = "(" ++ (intercalate "; \n" $ map prettyExp exps) ++ ")"
prettyExp (AssignExp v e _) = (prettyVar v) ++ " := " ++ (prettyExp e)
prettyExp (LetExp decs e _) = "let " ++ (intercalate "\n" $ map prettyDec decs) ++ " in " ++ (prettyExp e) ++ " end"
prettyExp (VarExp v) = prettyVar v
prettyExp (WhileExp e1 e2 _) = "while " ++ (prettyExp e1) ++ " do " ++ (prettyExp e2)
prettyExp (ForExp s _ e1 e2 e3 _) = "for " ++ s ++ " := " ++ (prettyExp e1) ++ " to " ++ (prettyExp e2) ++ " do " ++ (prettyExp e3)
prettyExp (ArrayExp s e1 e2 _) = s ++ " [" ++ (prettyExp e1) ++ "] of " ++ (prettyExp e2)
prettyExp (IfExp e1 e2 me3 _) =
  case me3 of
    Nothing -> "if " ++ (prettyExp e1) ++ " then " ++ (prettyExp e2)
    Just e3 -> "if " ++ (prettyExp e1) ++ " then " ++ (prettyExp e2) ++ " else " ++ (prettyExp e3)
prettyExp(RecordExp recs s _) = s ++ " { " ++ (intercalate ", " $ map prettyRecordField recs) ++ " } "

prettyRecordField :: (Symbol, Exp, Pos) -> String
prettyRecordField (s, e, _) = s ++ " = " ++ prettyExp e

prettyTy :: Ty -> String
prettyTy (NameTy s _) = s
prettyTy (ArrayTy s _) = "array of " ++ s
prettyTy (RecordTy fs) = "{" ++ (intercalate ", " $ map prettyField fs) ++ "}"

prettyField :: Field -> String
prettyField (Field f _ t _) = f ++ ": " ++ t

prettyVar :: Var -> String
prettyVar (SimpleVar s _) = s
prettyVar (FieldVar v s _) = (prettyVar v) ++ "." ++ s
prettyVar (SubscriptVar v exp _) = (prettyVar v) ++ "[" ++ (prettyExp exp) ++ "]"

prettyOp :: Oper -> String
prettyOp PlusOp = "+"
prettyOp MinusOp = "-"
prettyOp TimesOp = "*"
prettyOp DivideOp = "/"
prettyOp EqOp = "="
prettyOp NeqOp = "<>"
prettyOp LtOp = "<"
prettyOp LeOp = "<="
prettyOp GtOp = ">"
prettyOp GeOp = ">="

prettyDec :: Dec -> String
prettyDec (VarDec s _ ty exp _) =
  case ty of
    Nothing -> "var " ++ s ++ " := " ++ prettyExp exp
    Just t -> "var " ++ s ++ " : " ++ t ++ " := " ++ prettyExp exp
prettyDec (TypeDec decs) = intercalate "\n" $ map prettyTyDec decs
prettyDec (FunctionDec fundecs) = intercalate "\n" $ map prettyFuncDec fundecs

prettyTyDec :: (Symbol, Ty, Pos) -> String
prettyTyDec (s, t, _) = "type " ++ s ++ " = " ++ (prettyTy t)

prettyFuncDec :: FuncDec -> String
prettyFuncDec (FuncDec s fields mty exp _) =
  case mty of
    Nothing ->
      "function " ++ s ++ " ( " ++ (intercalate ", " $ map prettyField fields) ++ " ) = " ++ prettyExp exp
    Just ty ->
      "function " ++ s ++ " ( " ++ (intercalate ", " $ map prettyField fields) ++ " ) : " ++ ty ++ " = " ++ prettyExp exp
