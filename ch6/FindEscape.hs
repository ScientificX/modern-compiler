module FindEscape where

import qualified Abs
import qualified Symbol

import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map

type Depth = Integer
type EEnv = Map.Map (Symbol.Symbol, Depth) Bool

symb :: Abs.Symbol -> Symbol.Symbol
symb x = x

envMerge :: EEnv -> EEnv -> EEnv
envMerge = Map.unionWith (||)

envFind :: (Symbol.Symbol, Depth) -> EEnv -> Maybe ((Symbol.Symbol, Depth), Bool)
envFind (name, depth) eenv
  | depth < 0 = Nothing
  | otherwise =
    case Map.lookup (name, depth) eenv of
      Just b -> Just ((name, depth), b)
      Nothing ->
        envFind (name, depth - 1) eenv

traverseVar :: EEnv -> Depth -> Abs.Var -> (EEnv, Abs.Var)
traverseVar eenv d var@(Abs.SimpleVar s _) =
  let name = symb s in
    case envFind (name, d) eenv of
      Just (b, _) ->
        let eenv' = Map.insert b True eenv in
          (eenv', var)
      Nothing -> (eenv, var)
traverseVar eenv d var@(Abs.FieldVar v _ _) =
  let (veenv, _) = traverseVar eenv d v in
    (veenv, var)
traverseVar eenv d var@(Abs.SubscriptVar v expr _) =
  let
    (veenv, _) = traverseVar eenv d v
    (eeenv, _) = traverseExp eenv d expr
  in
    ((envMerge veenv eeenv), var)

traverseDec :: EEnv -> Depth -> Abs.Dec -> (EEnv, Abs.Dec)
traverseDec eenv _ dec@(Abs.TypeDec _) = (eenv, dec)
traverseDec eenv d dec@(Abs.VarDec name _ _ bodyExp _) =
  let
    eenv' = Map.insert (name, d) False eenv
    (beenv, _) = traverseExp eenv d bodyExp
  in
    ((envMerge eenv' beenv), dec)
traverseDec eenv d dec@(Abs.FunctionDec funcdecs) =
  let
    eenv' = foldl'
            (\e funcdec -> let (e', _) = traverseFuncdec e d funcdec in e')
            eenv
            funcdecs
  in
    (eenv', dec)

traverseFuncdec :: EEnv -> Depth -> Abs.FuncDec -> (EEnv, Abs.FuncDec)
traverseFuncdec eenv d (Abs.FuncDec name params ty expr pos) =
  let
    eenv' = Map.insert ((symb name), d) False eenv
    eenv'' = foldl'
             (\e (Abs.Field n _ _ _) -> Map.insert ((symb n), d) False e)
             eenv
             params
    (eeenv, _) = traverseExp eenv'' d expr
    params' = map
              (\f@(Abs.Field n e t p) ->
                case envFind ((symb n), d) eeenv of
                  Just (_, escape) -> (Abs.Field n escape t p)
                  Nothing -> f)
              params
    funcdec = Abs.FuncDec name params' ty expr pos
  in
    (eeenv, funcdec)

traverseExp :: EEnv -> Depth -> Abs.Exp -> (EEnv, Abs.Exp)
traverseExp eenv _ expr@(Abs.NilExp)         = (eenv, expr)
traverseExp eenv _ expr@(Abs.IntExp _)       = (eenv, expr)
traverseExp eenv _ expr@(Abs.StringExp _ _)  = (eenv, expr)
traverseExp eenv _ expr@(Abs.BreakExp _)     = (eenv, expr)
traverseExp eenv d expr@(Abs.OpExp l _ r _) =
  let
    (leenv, _) = traverseExp eenv d l
    (reenv, _) = traverseExp eenv d r
  in
    ((envMerge leenv reenv), expr)
traverseExp eenv d expr@(Abs.VarExp var) =
  let
    (veenv, _) = traverseVar eenv d var
  in
    (veenv, expr)
traverseExp eenv d expr@(Abs.RecordExp fields _ _) =
  let
    feenv = foldl'
            (\env (s, e, _) ->
              let
                (env', _) = traverseExp env d e
              in
                envMerge env' env)
            eenv
            fields
  in
    (feenv, expr)
traverseExp eenv d expr@(Abs.SeqExp exprs) =
  let
    eeenv = foldl'
            (\env e ->
              let
                (env', _) = traverseExp env d e
              in
                envMerge env' env)
            eenv
            exprs
  in
    (eeenv, expr)
traverseExp eenv d expr@(Abs.AssignExp var e _) =
  let
    (veenv, _) = traverseVar eenv d var
    (eeenv, _) = traverseExp eenv d e
  in
    ((envMerge veenv eeenv), expr)
traverseExp eenv d expr@(Abs.IfExp testExp thenExp elseExp _) =
  let
    (testeenv, _) = traverseExp eenv d testExp
    (theneenv, _) = traverseExp eenv d thenExp
    elseeenv =
      case elseExp of
        Just e ->
          let (ev, _) = traverseExp eenv d e in ev
        Nothing -> (Map.empty :: EEnv)
    eenv' = envMerge (envMerge testeenv theneenv) elseeenv
  in
    (eenv', expr)
traverseExp eenv d expr@(Abs.WhileExp testExp bodyExp _) =
  let
    (testeenv, _) = traverseExp eenv d testExp
    (bodyeenv, _) = traverseExp eenv d bodyExp
  in
    ((envMerge testeenv bodyeenv), expr)
traverseExp eenv d expr@(Abs.LetExp decs e _) =
  -- TODO see if vars in `decs` have been called in `e`
  let
    deenv = foldl'
            (\env dec ->
            let (denv, _) = traverseDec env (d + 1) dec in
              envMerge denv env)
            eenv
            decs
    (eeenv, _) = traverseExp deenv (d + 1) e
  in
    ((envMerge eeenv deenv), expr)
traverseExp eenv d expr@(Abs.ArrayExp _ sizeExp initExp _) =
  let
    (seenv, _) = traverseExp eenv d sizeExp
    (ieenv, _) = traverseExp eenv d initExp
  in
    ((envMerge seenv ieenv), expr)
traverseExp eenv d expr@(Abs.ForExp v _ assignExp limitExp bodyExp _) =
  -- TODO : check the `v` usage
  let
    (aeenv, _) = traverseExp eenv d assignExp
    (leenv, _) = traverseExp eenv d limitExp
    (beenv, _) = traverseExp eenv d bodyExp
  in
    ((envMerge (envMerge aeenv leenv) beenv), expr)
traverseExp eenv d expr@(Abs.CallExp name argExps _) =
  let
    aeenv = foldl'
            (\env argExp ->
              let (aenv, _) = traverseExp env d argExp in
                envMerge env aenv)
            eenv
            argExps
    eenv' = envMerge aeenv eenv
  in
    case envFind ((symb name), d) eenv' of
      Just (b, _) ->
        let env'' = Map.insert b True eenv' in
          (env'', expr)
      Nothing -> (eenv', expr)

findEscape :: Abs.Exp -> (EEnv, Abs.Exp)
findEscape expr = traverseExp (Map.empty :: EEnv) 0 expr
