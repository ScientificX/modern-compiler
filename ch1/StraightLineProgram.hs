module StraightLineProgram where

import Types
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.IO.Class

type Env = M.Map String Integer

interp :: Stm -> IO ()
interp stm = do
  putStrLn ""
  interpStm (M.empty) stm
  putStrLn ""

interpStm :: Env -> Stm -> IO Env
interpStm env (CompoundStm x y) = do
  xEnv <- interpStm env x
  interpStm xEnv y
interpStm env (AssignStm i exp) = do
  (val, expEnv) <- interpExp env exp
  return $ M.insert i val expEnv
interpStm env (PrintStm [e]) = do
  (val, expEnv) <- interpExp env e
  print $ val
  return env
interpStm env (PrintStm (e:es)) = do
  (val, expEnv) <- interpExp env e
  putStr $ show $ val
  putStr " "
  interpStm env (PrintStm es)

interpExp :: Env -> Exp -> IO (Integer, Env)
interpExp env (IdExp i) =
  case (M.lookup i env) of
    Just val -> return $ (val, env)
    Nothing -> error "can not find undefined value"
interpExp env (NumExp i) = return $ (i, env)
interpExp env (OpExp x op y) = do
  val <- interpBinOp env op x y
  return $ (val, env)
interpExp env (EseqExp stm exp) = do
  e <- interpStm env stm
  interpExp e exp

interpBinOp :: Env -> BinOp -> Exp -> Exp -> IO Integer
interpBinOp env op x y = do
  (xx, _) <- interpExp env x
  (yy, _) <- interpExp env y
  return $ binOp op xx yy

binOp :: BinOp -> Integer -> Integer -> Integer
binOp Plus = (+)
binOp Minus = (-)
binOp Times = (*)
binOp Divide = div

-- a := 5 + 3; b := (print(a, a-1), 10 * a); print(b)
program = (CompoundStm (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3))) (CompoundStm (AssignStm "b" (EseqExp (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)]) (OpExp (NumExp 10) Times (IdExp "a")))) (PrintStm [IdExp "b"])))

main = do
  interp program
