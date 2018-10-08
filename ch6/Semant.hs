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
import qualified Temp
import qualified Translate

import TypeError

type TypeResult = Either TypeError

type VEnv = Symbol.Table Env.EnvEntry
type TEnv = Symbol.Table Types.Ty

data ExpTy = ExpTy { ty :: Types.Ty } deriving (Show)

mkExpTy :: Types.Ty -> ExpTy
mkExpTy t = ExpTy { ty = t }

-- The functions that do all the work...
transProg :: Abs.Exp -> IO ()
transProg exp =
  let
    (mainLevel, temp) = Translate.newLevel Translate.outermost (Temp.namedLabel "main") [] Temp.create
  in
    case transExp Env.baseVEnv Env.baseTEnv mainLevel temp exp of
      Left err -> print err
      Right (ty, level, temp) -> do
        -- doin' this, at least for now
        putStrLn (show ty)
        putStrLn "\n\n"
        putStrLn (show level)

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
      (Types.TRecord _ x, Types.TRecord _ y) ->
        if x == y then return True else throwError $ TypeMismatch pos a' b'
      _ -> throwError $ TypeMismatch pos a' b'

compareTypes :: [Types.Ty] -> [Types.Ty] -> Abs.Pos -> TypeResult [Bool]
compareTypes x y pos = mapM (\(a, b) -> typeCheck a b pos) $ zip x y

-- TODO :: level + temp
typesForExpr :: VEnv -> TEnv -> Translate.Level -> Temp.Temp -> [Abs.Exp] -> TypeResult [Types.Ty]
typesForExpr venv tenv level temp exprs = do
  tys <- mapM (transExp venv tenv level temp) exprs
  return $ map (ty . expFst) tys
  where
    expFst (a, _, _) = a

transVar :: VEnv -> TEnv -> Translate.Level -> Temp.Temp -> Abs.Var -> TypeResult (ExpTy, Translate.Level, Temp.Temp)
transVar venv tenv level temp expression =
  case expression of
    (Abs.SimpleVar name pos) ->
      case Symbol.look venv name of
        Just (Env.VarEntry t _) -> do
          t' <- actualTy t pos
          return $ ((mkExpTy t'), level, temp)
        Nothing -> throwError $ UndefinedVariable pos name

    (Abs.FieldVar var name pos) -> do
      (var', level', temp') <- transVar venv tenv level temp var
      let v = ty var' in
        case v of
          (Types.TRecord fields _) ->
            case lookup name fields of
              Just t -> do
                t' <- actualTy t pos
                return $ ((mkExpTy t'), level', temp')
              Nothing -> throwError $ FieldNotFound pos name
          _ -> throwError $ FieldVarRequiresRecord pos v

    (Abs.SubscriptVar var exp pos) -> do
      (var', level', temp') <- transVar venv tenv level temp var
      v <- actualTy (ty var') pos
      case v of
        Types.TArray t _ -> do
          (e, level'', temp'') <- transExp venv tenv level' temp' exp
          let e' = ty e in
            case e' of
              Types.TInt -> return $ ((mkExpTy t), level'', temp'')
              _ -> throwError $ TypeMismatch pos Types.TInt e'
        _ -> throwError $ SubscriptVarRequiresArray pos v

data OpType = Arith | Comp | Eq deriving (Show)

opType :: Abs.Oper -> OpType
opType o =
  case o of
    Abs.PlusOp -> Arith
    Abs.MinusOp -> Arith
    Abs.TimesOp -> Arith
    Abs.DivideOp -> Arith
    Abs.EqOp -> Eq
    Abs.NeqOp -> Eq
    Abs.LtOp -> Comp
    Abs.LeOp -> Comp
    Abs.GtOp -> Comp
    Abs.GeOp -> Comp

transExp :: VEnv -> TEnv -> Translate.Level -> Temp.Temp -> Abs.Exp -> TypeResult (ExpTy, Translate.Level, Temp.Temp)
transExp venv tenv level temp expression =
  case expression of
    (Abs.NilExp) -> return $ ((mkExpTy Types.TNil), level, temp)

    (Abs.IntExp _) -> return $ ((mkExpTy Types.TInt), level, temp)

    (Abs.StringExp _ _) -> return $ ((mkExpTy Types.TString), level, temp)

    (Abs.VarExp var) -> transVar venv tenv level temp var

    (Abs.BreakExp _) -> return $ ((mkExpTy Types.TUnit), level, temp)

    (Abs.CallExp name exprs pos) ->
      case Symbol.look venv name of
        Just (Env.FunEntry a r _ _) ->
          if length a == length exprs then
            do
              e <- typesForExpr venv tenv level temp exprs
              compareTypes a e pos
              return $ ((mkExpTy r), level, temp)
          else
            throwError $ ArgumentCountMismatch pos name (length a) (length exprs)
        Just (Env.VarEntry _ _) -> throwError $ NotAFunction pos name
        Nothing -> throwError $ UndefinedFunction pos name

    (Abs.OpExp l op r pos) -> do
      (left, level', temp') <- trexp level temp l
      (right, level'', temp'') <- trexp level' temp' r
      case opType op of
        Arith -> do
          typeCheck Types.TInt (ty left) pos
          typeCheck Types.TInt (ty right) pos
          return $ ((mkExpTy Types.TInt), level'', temp'')
        Comp ->
          case (ty left) of
            Types.TInt -> do
              typeCheck (ty left) (ty right) pos
              return $ ((mkExpTy Types.TInt), level'', temp'')
            Types.TString -> do
              typeCheck (ty left) (ty right) pos
              return $ ((mkExpTy Types.TInt), level'', temp'')
            _ -> throwError $ InvalidOp (ty left) (ty right) (show (opType op)) pos
        Eq ->
          case (ty left) of
            Types.TInt -> do
              typeCheck (ty left) (ty right) pos
              return $ ((mkExpTy Types.TInt), level'', temp'')
            Types.TString -> do
              typeCheck (ty left) (ty right) pos
              return $ ((mkExpTy Types.TInt), level'', temp'')
            Types.TArray _ _ -> do
              typeCheck (ty left) (ty right) pos
              return $ ((mkExpTy Types.TInt), level'', temp'')
            Types.TRecord _ _ -> do
              typeCheck (ty left) (ty right) pos
              return $ ((mkExpTy Types.TInt), level'', temp'')
            Types.TNil -> do
              typeCheck (ty left) (ty right) pos
              return $ ((mkExpTy Types.TInt), level'', temp'')
            _ -> throwError $ InvalidOp (ty left) (ty right) (show (opType op)) pos

    (Abs.RecordExp recordFields recordType pos) ->
      case Symbol.look tenv recordType of
        Just t -> do
          t' <- actualTy t pos
          case t' of
            Types.TRecord fs u ->
              do
                f <- typesForExpr venv tenv level temp (map (\(_, e, _) -> e) recordFields)
                compareTypes f (map snd fs) pos
                return $ ((mkExpTy $ Types.TRecord fs u), level, temp)
            t -> throwError $ RecordTypeNotFound pos t
        Nothing -> throwError $ TypeNotFound pos recordType

    (Abs.SeqExp exprs) ->
      case exprs of
        [] -> return $ ((mkExpTy $ Types.TUnit), level, temp)
        _ -> do
          -- (mkExpTy Types.TUnit) is a throwaway. we dont care about the type until the end. hence (_, l, t). yes...it's gross.
          (ty, level', temp') <- foldM (\(_, l, t) e -> transExp venv tenv l t e) ((mkExpTy Types.TUnit), level, temp) exprs
          return $ (ty, level', temp')

    (Abs.AssignExp var exp pos) -> do
      (e, level', temp') <- trexp level temp exp
      (v, level'', temp'') <- transVar venv tenv level' temp' var
      typeCheck (ty e) (ty v) pos
      return $ ((mkExpTy Types.TUnit), level'', temp'')

    (Abs.IfExp testE trueE falseE pos) -> do
      (testExpTy, level', temp') <- trexp level temp testE
      let testTy = ty testExpTy in
        case testTy of
          Types.TInt -> do
            (trueExpTy, level'', temp'') <- trexp level' temp' trueE
            case falseE of
              Just fE -> do
                (falseExpTy, l, t) <- trexp level'' temp'' fE
                typeCheck (ty falseExpTy) (ty trueExpTy) pos
                return $ ((mkExpTy $ ty trueExpTy), l, t)
              Nothing -> do
                typeCheck Types.TUnit (ty trueExpTy) pos
                return $ ((mkExpTy $ Types.TUnit), level'', temp'')
          _ -> throwError $ IfExpectsTruth pos testTy

    (Abs.WhileExp testExp bodyExp pos) -> do
      (testExpTy, level', temp') <- trexp level temp testExp
      (bodyExpTy, level'', temp'') <- trexp level' temp' bodyExp
      typeCheck Types.TInt (ty testExpTy) pos
      typeCheck Types.TUnit (ty bodyExpTy) pos
      return $ ((mkExpTy $ Types.TUnit), level'', temp'')

    (Abs.LetExp decs exp pos) -> do
      -- TODO level fold
      (venv', tenv', level', temp') <- foldlM (\(ve, te, l, t) -> transDec ve te l t) (venv, tenv, level, temp) decs
      (expTy, level'', temp'') <- trace (show venv') $ transExp venv' tenv' level' temp' exp
      return $ ((mkExpTy (ty expTy)), level'', temp'')

    (Abs.ArrayExp arrayTy size initExp pos) ->
      case Symbol.look tenv arrayTy of
        Just t -> do
          t' <- actualTy t pos
          case t' of
            (Types.TArray arrayTy u) -> do
              (sizeExpTy, level', temp') <- trexp level temp size
              (initExpTy, level'', temp'') <- trexp level' temp' initExp
              typeCheck Types.TInt (ty sizeExpTy) pos
              typeCheck arrayTy (ty initExpTy) pos
              return $ ((mkExpTy t'), level'', temp'')
            _ -> throwError $ UnknownArrayType pos t'
        Nothing -> throwError $ TypeNotFound pos arrayTy

    (Abs.ForExp name _ assignExp limitExp bodyExp pos) ->
      -- translate to `let`/`while`
      let
        ivar = Abs.SimpleVar name pos
        limitvar = Abs.SimpleVar "limit" pos
        decs = [ (Abs.VarDec name False Nothing assignExp pos)
               , (Abs.VarDec "limit" False Nothing limitExp pos) ]
        loopTest = Abs.OpExp (Abs.VarExp ivar) Abs.LeOp (Abs.VarExp limitvar) pos
        loopIncrement = Abs.OpExp (Abs.VarExp ivar) Abs.PlusOp (Abs.IntExp 1) pos
        seqExp = Abs.SeqExp [ bodyExp , (Abs.AssignExp ivar loopIncrement pos) ]
        loop = Abs.WhileExp loopTest seqExp pos
      in
        trexp level temp (Abs.LetExp decs loop pos)
  where
    trexp level temp e = transExp venv tenv level temp e


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

transFunctionDec :: VEnv -> TEnv -> Translate.Level -> Temp.Temp -> Abs.FuncDec -> TypeResult (VEnv, TEnv, Translate.Level, Temp.Temp)
transFunctionDec venv tenv level temp (Abs.FuncDec name params result body pos) = do
  let paramNames = fieldNames params in
    if length paramNames == length (nub paramNames) then
      do
        tys <- fieldTypes tenv params
        let (venvParam, _, _) = foldr (\b (v, l, t) -> transParam b v l t) (venv, level, temp) (zip params tys) in
          let
            escapes = map findEscape params
            (label, temp'') = Temp.newLabel temp
            (level'', newTemp) = Translate.newLevel level label escapes temp''
          in
            do
              functionTy <- functionType tenv result pos
              let venv' = Symbol.enter venvParam name (Env.FunEntry tys functionTy level'' label) in
                do
                  (bodyTy, _, _) <- transExp venv' tenv level'' newTemp body
                  if (ty bodyTy) == functionTy then
                    return $ ((Symbol.enter venv name (Env.FunEntry tys functionTy level'' label)), tenv, level'', newTemp)
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

    transParam :: (Abs.Field, Types.Ty) -> VEnv -> Translate.Level -> Temp.Temp -> (VEnv, Translate.Level, Temp.Temp)
    transParam ((Abs.Field n escape _ _), t) venv level temp =
      let
        (access, level', temp') = Translate.allocLevel level escape temp
      in
        (Symbol.enter venv n (Env.VarEntry t access), level', temp')

    findEscape :: Abs.Field -> Bool
    findEscape (Abs.Field _ escape _ _) = escape

transTypeDec :: VEnv -> TEnv -> (Abs.Symbol, Abs.Ty, Abs.Pos) -> TypeResult (VEnv, TEnv)
transTypeDec venv tenv (name, ty, pos) = do
  te <- translateName tenv (name, ty, pos)
  ty' <- transTy te ty
  case ty' of
    Types.TName _ Nothing -> throwError $ TypeNotFound pos (show ty)
    _ ->
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

functionHeader :: VEnv -> TEnv -> Translate.Level -> Abs.FuncDec -> TypeResult (VEnv, TEnv)
functionHeader venv tenv level f@(Abs.FuncDec name fields _ _ _) = do
  returnType <- functionReturnType tenv f
  fieldTypes <- fieldTypes tenv fields
  let venv' = Symbol.enter venv name $ (Env.FunEntry fieldTypes returnType level (Temp.namedLabel name)) in
    return $ (venv', tenv)

transDec :: VEnv -> TEnv -> Translate.Level -> Temp.Temp -> Abs.Dec -> TypeResult (VEnv, TEnv, Translate.Level, Temp.Temp)
transDec venv tenv level temp (Abs.FunctionDec fundecs) = do
  (venv', tenv') <- foldM (\(v, t) -> functionHeader v t level) (venv, tenv) fundecs
  foldlM (\(ve, te, l, t) -> transFunctionDec ve te l t) (venv', tenv', level, temp) fundecs
transDec venv tenv level temp (Abs.VarDec name escape typ exp pos) = do
  (expTy, level', temp') <- transExp venv tenv level temp exp
  let et = ty expTy in
    let (access, level'', temp'') = Translate.allocLevel level' escape temp' in
      case typ of
        Nothing ->
          if et == Types.TNil then
            throwError $ InvalidNil pos
          else
            return $ (Symbol.enter venv name (Env.VarEntry et access), tenv, level', temp')
        Just typeName ->
          case Symbol.look tenv typeName of
            Nothing -> throwError $ TypeNotFound pos typeName
            Just t -> do
              typeCheck t et pos
              return $ (Symbol.enter venv name (Env.VarEntry et access), tenv, level', temp')
transDec venv tenv level temp (Abs.TypeDec decs) =
  -- TODO : check duplicate type definitions
  let
    tenv' = foldr (\(n, _, _) te -> Symbol.enter te n (Types.TName n Nothing)) tenv decs
  in
    do
      (v, t) <- (foldM (\(v, t) -> transTypeDec v t) (venv, tenv') decs)
      return $ (v, t, level, temp)

translateName :: TEnv -> (Abs.Symbol, Abs.Ty, Abs.Pos) -> TypeResult TEnv
translateName tenv (name, ty, _) =
  case Symbol.look tenv name of
    Just (Types.TName n _) -> do
      t <- transTy tenv ty
      return $ Symbol.enter tenv n (Types.TName n (Just t))
    _ -> return $ tenv

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
