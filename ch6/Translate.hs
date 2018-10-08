module Translate (newLevel, allocLevel, outermost, Level(..), Access(..)) where

import qualified Temp
import qualified Frame
import qualified LLVMFrame as Frame


-- Level parent name formals frame
data Level = Level Level Temp.Label [Bool] Frame.LFrame
           | Outermost
           deriving (Eq, Show)

-- Access level access
data Access = Access Level Frame.Access deriving (Show)

outermost = Outermost

newLevel :: Level -> Temp.Label -> [Bool] -> Temp.Temp -> (Level, Temp.Temp)
newLevel parent name formals temp =
  let
    (label, temp') = Temp.newLabel temp
    (frame, temp'') = Frame.newFrame label formals temp'
  in
    ((Level parent label formals frame), temp'')

allocLevel :: Level -> Bool -> Temp.Temp -> (Access, Level, Temp.Temp)
allocLevel level@(Level parent name formals frame) escapes temp =
  let
    (access, frame', temp') = Frame.allocLocal frame escapes temp
  in
    ((Access level access), (Level parent name formals frame'), temp')
