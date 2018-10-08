module LLVMFrame (LFrame(..)) where

import qualified Frame
import qualified Temp

-- LFrame name formals locals
data LFrame = LFrame Temp.Label [Frame.Access] [Frame.Access] deriving (Eq, Show)

instance Frame.FrameBase LFrame where
  newFrame = newFrame'
  formals (LFrame _ f _) = f
  allocLocal = allocLocal'

newFrame' :: Temp.Label -> [Bool] -> Temp.Temp -> (LFrame, Temp.Temp)
newFrame' label formals temp =
  let
    createFormals (t, fs) (escapes, n) =
      if escapes then
        (t, (Frame.InFrame (-n) : fs))
      else
        let (regInt, t') = Temp.newTemp t in
          (t', ((Frame.InReg regInt) : fs))

    (temp', formals') = foldl createFormals (temp, []) $ zip formals [0..]
  in
    ((LFrame label formals' []), temp')

allocLocal' :: LFrame -> Bool -> Temp.Temp -> (Frame.Access, LFrame, Temp.Temp)
allocLocal' (LFrame name formals locals) escape temp =
  let
    (f, t) =
      if escape then
        let lengthLocals = length locals in
          ((Frame.InFrame (lengthLocals + 1)), temp)
      else
        let (regInt, newTemp) = Temp.newTemp temp in
          ((Frame.InReg regInt), newTemp)
  in
    (f, (LFrame name formals (f : locals)), t)
