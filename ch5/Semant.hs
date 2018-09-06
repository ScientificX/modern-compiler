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
               | UnknownFunctionParamterType Abs.Pos
               | FunctionTypeInvalid Types.Ty Types.Ty Abs.Pos
               | UndefinedType String Abs.Pos
               | FunctionTypeNotFound Abs.Pos
               | UnknownError String

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

-- The functions that do all the work...
transProg :: Abs.Exp -> IO ()
transProg exp =
  case transExp Env.baseVEnv Env.baseTEnv exp of
    Left err -> print err
    Right _ -> print "ok."

actualTy :: Types.Ty -> Abs.Pos -> TypeResult Types.Ty
actualTy (Types.TName n (Just t)) pos = actualTy t pos
actualTy (Types.TName n Nothing) pos = throwError $ TypeNotFound pos n
actualTy (Types.TArray t u) pos = do
  t' <- actualTy t pos
  return $ Types.TArray t' u
actualTy t _ = return $ t

typeCheck :: Types.Ty -> Types.Ty -> Abs.Pos -> TypeResult Bool
typeCheck a b pos = do
  a' <- actualTy a pos
  b' <- actualTy b pos
  if a' == b' then
    return True
  else
    case (a', b') of
      (Types.TRecord _ _, Types.TNil) -> return True
      (Types.TNil, Types.TRecord _ _) -> return True
      _ -> throwError $ TypeMismatch pos a' b'

compareTypes :: [Types.Ty] -> [Types.Ty] -> Abs.Pos -> TypeResult [Bool]
compareTypes x y pos = mapM (\(a, b) -> typeCheck a b pos) $ zip x y

typesForExpr :: VEnv -> TEnv -> [Abs.Exp] -> TypeResult [Types.Ty]
typesForExpr venv tenv exprs = do
  tys <- mapM (transExp venv tenv) exprs
  return $ map ty tys

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
                return $ mkExpTy $ ty trueExpTy
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


fieldNames :: [Abs.Field] -> [Symbol.Symbol]
fieldNames = map (\(Abs.Field n _ _ _) -> n)

fieldTypes :: TEnv -> [Abs.Field] -> TypeResult [Types.Ty]
fieldTypes tenv fields = mapM (fty tenv) fields
  where
    fty :: TEnv -> Abs.Field -> TypeResult Types.Ty
    fty tenv (Abs.Field _ _ t p) =
      case Symbol.look tenv t of
        Just t -> return t
        Nothing -> throwError $ UndefinedType t p

-- TODO : recurive function definition
transFunctionDec :: VEnv -> TEnv -> Abs.FuncDec -> TypeResult (VEnv, TEnv)
transFunctionDec venv tenv (Abs.FuncDec name params result body pos) = do
  let paramNames = fieldNames params in
    if length paramNames == length (nub paramNames) then
      do
        tys <- fieldTypes tenv params
        let venvParam = foldr transParam venv (zip paramNames tys) in
          do
            functionTy <- functionType tenv result pos
            let venv' = Symbol.enter venvParam name (Env.FunEntry tys functionTy) in
              do
                bodyTy <- transExp venv' tenv body
                if (ty bodyTy) == functionTy then
                  return $ (venv', tenv)
                else
                  throwError $ FunctionTypeInvalid functionTy (ty bodyTy) pos
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
  te <- translateName tenv (name, ty, pos)
  ty' <- transTy te ty
  let tenv' = Symbol.enter te name ty' in
    return $ (venv, tenv')

functionReturnType :: TEnv -> Abs.FuncDec -> TypeResult Types.Ty
functionReturnType tenv (Abs.FuncDec _ _ result _ pos) =
  case result of
    Just r ->
      case Symbol.look tenv r of
        Just ty -> return ty
        Nothing -> throwError $ UndefinedType r pos
    Nothing -> return Types.TUnit

functionHeader :: VEnv -> TEnv -> Abs.FuncDec -> TypeResult (VEnv, TEnv)
functionHeader venv tenv f@(Abs.FuncDec name fields _ _ _) = do
  returnType <- functionReturnType tenv f
  fieldTypes <- fieldTypes tenv fields
  let venv' = Symbol.enter venv name $ (Env.FunEntry fieldTypes returnType) in
    return $ (venv', tenv)

transDec :: VEnv -> TEnv -> Abs.Dec -> TypeResult (VEnv, TEnv)
transDec venv tenv (Abs.FunctionDec fundecs) = do
  (venv', tenv') <- foldM (\(v, t) -> functionHeader v t) (venv, tenv) fundecs
  foldlM (\(v, t) -> transFunctionDec v t) (venv', tenv') fundecs
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
  -- TODO : check duplicate type definitions
  let
    tenv' = foldr (\(n, _, _) te -> Symbol.enter te n (Types.TName n Nothing)) tenv decs
    -- tenv'' = foldM (\t d -> translateName t d) tenv' decs
  in
    do
      -- te <- tenv''
      -- te' <- foldM (\t d -> translateNameWithTyName t d) te decs
      foldM (\(v, t) -> transTypeDec v t) (venv, tenv') decs

translateName :: TEnv -> (Abs.Symbol, Abs.Ty, Abs.Pos) -> TypeResult TEnv
translateName tenv (name, ty, _) =
  case Symbol.look tenv name of
    Just (Types.TName n _) -> do
      t <- transTy tenv ty
      return $ Symbol.enter tenv n (Types.TName n (Just t))
    _ -> return $ tenv

-- translateNameWithTyName :: TEnv -> (Abs.Symbol, Abs.Ty, Abs.Pos) -> TypeResult TEnv
-- translateNameWithTyName tenv (name, ty, _) =
--   case Symbol.look tenv name of
--     Just (Types.TName n _) -> do
--       t <- transTyName tenv ty
--       return $ Symbol.enter tenv n (Types.TName n (Just t))
--     _ -> return $ tenv

-- transTyName :: TEnv -> Abs.Ty -> TypeResult Types.Ty
-- transTyName tenv (Abs.NameTy symbol pos) =
--   case Symbol.look tenv symbol of
--     Just t ->
--       case t of
--         Types.TName s _ ->
--           case Symbol.look tenv s of
--             Just (Types.TName s' (Just (Types.TName s'' _))) -> do
--               fT <- followTy tenv [symbol] s''
--               return $ Types.TName s' (Just fT)
--             Just ty -> return $ ty
--             _ -> throwError $ UnknownError "transTyName gone bad"
--         _ -> return $ t
--     _ -> throwError $ UnknownError "transTyName can't find"
-- transTyName tenv t = transTy tenv t

-- followTy :: TEnv -> [Abs.Symbol] -> Abs.Symbol -> TypeResult Types.Ty
-- followTy tenv seen sym =
--   if all (/= sym) seen then
--     case Symbol.look tenv sym of
--       Just ty ->
--         case ty of
--           Types.TName s (Just (Types.TName s' _)) -> do
--             t <- followTy tenv (s:seen) s'
--             return $ Types.TName s (Just t)
--           _ -> return $ ty
--       _ -> throwError $ UnknownError "followTy whoops"
--   else
--     throwError $ UnknownError "followTy gone wrong"

transTy :: TEnv -> Abs.Ty -> TypeResult Types.Ty
transTy tenv (Abs.NameTy symbol pos) =
  case Symbol.look tenv symbol of
    Just ty -> return ty
    Nothing -> throwError $ UndefinedType symbol pos
transTy tenv (Abs.RecordTy fields) = do
  -- TODO check duplicate fields? -- length paramNames == length (nub paramNames)
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

generateUnique :: Abs.Pos -> Integer
generateUnique (Abs.Pos line column) = fromIntegral (line * 1000 + column)
