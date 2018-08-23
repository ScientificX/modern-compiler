module Semant where

import Prelude hiding (exp)
import Control.Monad.Except
import Control.Monad
import Data.Foldable
import Data.List (nub)

import Debug.Trace (trace)

import qualified Symbol
import qualified Abs
import qualified Env
import qualified Types

type VEnv = Symbol.Table Env.EnvEntry
type TEnv = Symbol.Table Types.Ty

-- Translate comes in a later chapter so we have a placeholder for now
type Translate = ()
data ExpTy = ExpTy { exp :: Translate, ty :: Types.Ty } deriving (Show)

mkExpTy :: Types.Ty -> ExpTy
mkExpTy t = ExpTy { exp = (), ty = t }

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
               | UnkownFunctionParamterType Abs.Pos
               | FunctionTypeInvalid Types.Ty Types.Ty Abs.Pos
               | UndefinedType String Abs.Pos

type TypeResult = Either TypeError

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
    showError pos $ "Unkown type for Array: " ++ show t
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
  show (UnkownFunctionParamterType pos) =
    showError pos $ "Unkown function parameter type"
  show (FunctionTypeInvalid fnTy resultTy pos) =
    showError pos $ "Function declared as " ++ show fnTy ++ " but returned " ++ show resultTy
  show (UndefinedType ty pos) =
    showError pos $ "Undefined type: " ++ ty

-- The functions that do all the work...

actualTy :: Types.Ty -> Abs.Pos -> TypeResult Types.Ty
actualTy (Types.TName n (Just t)) pos = actualTy t pos
actualTy (Types.TName n Nothing) pos = throwError $ TypeNotFound pos n
actualTy (Types.TArray t u) pos = do
  t' <- actualTy t pos
  return $ Types.TArray t' u
actualTy t _ = return $ t

-- TODO : check results or use `if` statements instead of silly imperative style usage
typeCheck :: Types.Ty -> Types.Ty -> Abs.Pos -> TypeResult Bool
typeCheck a b pos = do
  a' <- actualTy a pos
  b' <- actualTy b pos
  if a' == b' then
    return True
  else
    throwError $ TypeMismatch pos a' b'

compareTypes :: [Types.Ty] -> [Types.Ty] -> Abs.Pos -> TypeResult [Bool]
compareTypes x y pos = mapM (\(a, b) -> typeCheck a b pos) $ zip x y

typesForExpr :: VEnv -> TEnv -> [Abs.Exp] -> TypeResult [Types.Ty]
typesForExpr venv tenv exprs = do
  tys <- mapM (transExp venv tenv) exprs
  return $ map ty tys

-- TODO : make `transProg` return a value and not `print`
transProg :: Abs.Exp -> IO ()
transProg exp =
  case transExp Env.baseVEnv Env.baseTEnv exp of
    Left err -> print err
    Right _ -> print "ok."

transVar :: VEnv -> TEnv -> Abs.Var -> TypeResult ExpTy
transVar venv tenv expression =
  case expression of
    (Abs.SimpleVar name pos) ->
      case Symbol.look venv name of
        Just (Env.VarEntry t) -> do
          t' <- actualTy t pos
          return $ mkExpTy t'
        Nothing -> throwError $ UndefinedVariable pos name

    (Abs.FieldVar var name pos) -> do
      var' <- transVar venv tenv var
      let v = ty var' in
        case v of
          (Types.TRecord fields _) ->
            case lookup name fields of
              Just t -> do
                t' <- actualTy t pos
                return $ mkExpTy t'
              Nothing -> throwError $ FieldNotFound pos name
          _ -> throwError $ FieldVarRequiresRecord pos v

    (Abs.SubscriptVar var exp pos) -> do
      var' <- transVar venv tenv var
      v <- actualTy (ty var') pos
      case v of
        Types.TArray t _ -> do
          e <- transExp venv tenv exp
          let e' = ty e in
            case e' of
              Types.TInt -> return $ mkExpTy t
              _ -> throwError $ TypeMismatch pos Types.TInt e'
        _ -> throwError $ SubscriptVarRequiresArray pos v

transExp :: VEnv -> TEnv -> Abs.Exp -> TypeResult ExpTy
transExp venv tenv expression =
  case expression of
    (Abs.NilExp) -> return $ mkExpTy Types.TNil

    (Abs.IntExp _) -> return $ mkExpTy Types.TInt

    (Abs.StringExp _ _) -> return $ mkExpTy Types.TString

    (Abs.VarExp var) -> transVar venv tenv var

    (Abs.BreakExp _) -> return $ mkExpTy Types.TUnit

    (Abs.CallExp name exprs pos) ->
      case Symbol.look venv name of
        Just (Env.FunEntry a r) ->
          if length a == length exprs then
            do
              e <- typesForExpr venv tenv exprs
              compareTypes a e pos
              return $ mkExpTy r
          else
            throwError $ ArgumentCountMismatch pos name (length a) (length exprs)
        Just (Env.VarEntry _) -> throwError $ NotAFunction pos name
        Nothing -> throwError $ UndefinedFunction pos name

    (Abs.OpExp l op r pos) -> do
      left <- trexp l
      right <- trexp r
      typeCheck Types.TInt (ty left) pos
      typeCheck Types.TInt (ty right) pos
      return $ mkExpTy Types.TInt

    (Abs.RecordExp recordFields recordType pos) ->
      case Symbol.look tenv recordType of
        Just t -> do
          t' <- actualTy t pos
          case t' of
            Types.TRecord fs u ->
              do
                f <- typesForExpr venv tenv (map (\(_, e, _) -> e) recordFields)
                compareTypes f (map snd fs) pos
                return $ mkExpTy $ Types.TRecord fs u
            t -> throwError $ RecordTypeNotFound pos t
        Nothing -> throwError $ TypeNotFound pos recordType

    (Abs.SeqExp exprs) ->
      case exprs of
        [] -> return $ mkExpTy $ Types.TUnit
        _ -> do
          tys <- mapM trexp exprs
          return $ last tys

    (Abs.AssignExp var exp pos) -> do
      e <- trexp exp
      v <- transVar venv tenv var
      typeCheck (ty e) (ty v) pos
      return $ mkExpTy Types.TUnit

    (Abs.IfExp testE trueE falseE pos) -> do
      testExpTy <- trexp testE
      let testTy = ty testExpTy in
        case testTy of
          Types.TInt -> do
            trueExpTy <- trexp trueE
            case falseE of
              Just fE -> do
                falseExpTy <- trexp fE
                typeCheck (ty falseExpTy) (ty trueExpTy) pos
                return $ mkExpTy $ Types.TUnit
              Nothing -> do
                typeCheck Types.TUnit (ty trueExpTy) pos
                return $ mkExpTy $ Types.TUnit
          _ -> throwError $ IfExpectsTruth pos testTy

    (Abs.WhileExp testExp bodyExp pos) -> do
      testExpTy <- trexp testExp
      bodyExpTy <- trexp bodyExp
      typeCheck Types.TInt (ty testExpTy) pos
      typeCheck Types.TUnit (ty bodyExpTy) pos
      return $ mkExpTy $ Types.TUnit

    (Abs.LetExp decs exp pos) -> do
      (venv', tenv') <- foldlM (\(v, t) -> transDec v t) (venv, tenv) decs
      expTy <- transExp venv' tenv' exp
      return $ mkExpTy (ty expTy)

    (Abs.ArrayExp arrayTy size initExp pos) ->
      case Symbol.look tenv arrayTy of
        Just t -> do
          t' <- actualTy t pos
          case t' of
            (Types.TArray arrayTy u) -> do
              sizeExpTy <- trexp size
              initExpTy <- trexp initExp
              typeCheck Types.TInt (ty sizeExpTy) pos
              typeCheck arrayTy (ty initExpTy) pos
              return $ mkExpTy t'
            _ -> throwError $ UnknownArrayType pos t'
        Nothing -> throwError $ TypeNotFound pos arrayTy

    -- (Abs.ForExp name assignExp limitExp bodyExp pos) =
      -- TODO


    _ -> return $ mkExpTy Types.TUnit
  where
    trexp e = transExp venv tenv e

-- TODO : recurive function definition
transFunctionDec :: VEnv -> TEnv -> Abs.FuncDec -> TypeResult (VEnv, TEnv)
transFunctionDec venv tenv (Abs.FuncDec name params result body pos) = do
  let paramNames = map (\(Abs.Field n _ _ _) -> n) params in
    if length paramNames == length (nub paramNames) then
      let
        paramTypeNames = map (\(Abs.Field _ _ t _) -> t) params
        paramTypes = mapM (\t -> Symbol.look tenv t) paramTypeNames
      in
        case paramTypes of
          Just tys ->
            let venvParam = foldr transParam venv (zip paramNames tys) in
              do
                bodyTy <- transExp venvParam tenv body
                functionTy <- functionType tenv result pos
                if (ty bodyTy) == functionTy then
                  let venvFun = Symbol.enter venv name (Env.FunEntry tys functionTy) in
                    return $ (venvFun, tenv)
                else
                  throwError $ FunctionTypeInvalid functionTy (ty bodyTy) pos
          Nothing ->
            throwError $ UnkownFunctionParamterType pos
    else
      throwError $ DuplicateFunctionParameters pos
  where
    functionType :: TEnv -> Maybe Abs.Symbol -> Abs.Pos -> TypeResult Types.Ty
    functionType tenv result pos =
      case result of
        Nothing -> return $ Types.TUnit
        Just ty ->
          case Symbol.look tenv ty of
            Nothing -> throwError $ TypeNotFound pos ty
            Just t -> return $ t

    transParam :: (Abs.Symbol, Types.Ty) -> VEnv -> VEnv
    transParam (n, t) venv = Symbol.enter venv n (Env.VarEntry t)

transTypeDec :: VEnv -> TEnv -> (Abs.Symbol, Abs.Ty, Abs.Pos) -> TypeResult (VEnv, TEnv)
transTypeDec venv tenv (name, ty, pos) = do
  -- TODO: recursive definition. insert "header" info into `tenv` and use in `transTy` call
  ty' <- transTy tenv ty
  let tenv' = Symbol.enter tenv name ty' in
    return $ (venv, tenv')

transDec :: VEnv -> TEnv -> Abs.Dec -> TypeResult (VEnv, TEnv)
transDec venv tenv (Abs.FunctionDec fundecs) =
  foldlM (\(v, t) -> transFunctionDec v t) (venv, tenv) fundecs
transDec venv tenv (Abs.VarDec name _ typ exp pos) = do
  expTy <- transExp venv tenv exp
  let et = ty expTy in
    case typ of
      Nothing ->
        if et == Types.TNil then
          throwError $ InvalidNil pos
        else
          return $ (Symbol.enter venv name (Env.VarEntry et), tenv)
      Just typeName ->
        case Symbol.look tenv typeName of
          Nothing -> throwError $ TypeNotFound pos typeName
          Just t -> do
            typeCheck t et pos
            return $ (Symbol.enter venv name (Env.VarEntry et), tenv)
transDec venv tenv (Abs.TypeDec decs) =
  foldM (\(v, t) -> transTypeDec v t) (venv, tenv) decs

transTy :: TEnv -> Abs.Ty -> TypeResult Types.Ty
transTy tenv (Abs.NameTy symbol pos) =
  case Symbol.look tenv symbol of
    -- TODO - use `actualTy` - deal with recursive definition
    Just ty -> return ty
    Nothing -> throwError $ UndefinedType symbol pos
transTy tenv (Abs.RecordTy fields) = do
  -- TODO check duplicate fields? -- length paramNames == length (nub paramNames)
  -- TODO recursive record definitions?
  -- TODO better Abs.Pos ?
  -- TODO add field names to tenv/venv ?
  recordFields <- mapM (transTyField tenv) fields
  let (Abs.Field _ _ _ pos) = (head fields) in
    return $ Types.TRecord recordFields (generateUnique pos)
transTy tenv (Abs.ArrayTy symbol pos) =
  case Symbol.look tenv symbol of
    Just ty -> return $ Types.TArray ty (generateUnique pos)
    Nothing -> throwError $ UndefinedType symbol pos

transTyField :: TEnv -> Abs.Field -> TypeResult (Symbol.Symbol, Types.Ty)
transTyField tenv (Abs.Field symbol _ ty pos) =
  case Symbol.look tenv ty of
    Just ty -> return $ (symbol, ty)
    Nothing -> throwError $ UndefinedType ty pos

-- TODO: make better? /shrug
generateUnique :: Abs.Pos -> Integer
generateUnique (Abs.Pos line column) = fromIntegral (line * 1000 + column)