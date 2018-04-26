module MaxArgs where

import Types

maxargs :: Stm -> Int
maxargs (CompoundStm x y) = maxargs x `max` maxargs y
maxargs (AssignStm _ exp) = maxargsExp exp
maxargs (PrintStm []) = 0
maxargs (PrintStm exps) = length exps `max` (maximum $ map maxargsExp exps)

maxargsExp :: Exp -> Int
maxargsExp (OpExp x _ y) = maxargsExp x `max` maxargsExp y
maxargsExp (EseqExp stm exp) = maxargs stm `max` maxargsExp exp
maxargsExp _ = 0

-- a := 5 + 3; b := (print(a, a-1), 10 * a); print(b)
program = (CompoundStm (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3))) (CompoundStm (AssignStm "b" (EseqExp (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)]) (OpExp (NumExp 10) Times (IdExp "a")))) (PrintStm [IdExp "b"])))
-- maxargs program == 2
threeArgPrint = PrintStm [NumExp 1, EseqExp (PrintStm [NumExp 1, NumExp 2, NumExp 3]) (NumExp 1)]
-- maxargs threeArgPrint == 3
fourArgPrint = PrintStm [NumExp 1, EseqExp (PrintStm [NumExp 1, NumExp 2, NumExp 3]) (EseqExp (PrintStm [NumExp 1, NumExp 2, NumExp 3, NumExp 4]) (NumExp 1))]
-- maxargs fourArgPrint == 4

main = do
  print $ maxargs program == 2
  print $ maxargs threeArgPrint == 3
  print $ maxargs fourArgPrint == 4
