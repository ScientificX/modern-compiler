module Temp ( Label(..)
            , Temp(..)
            , create
            , newTemp
            , makeString
            , newLabel
            , namedLabel) where

import qualified Symbol

type Label = Symbol.Symbol

-- Temp temps labels
data Temp = Temp Int Int deriving (Eq, Show)

create :: Temp
create = Temp 0 0

newTemp :: Temp -> (Int, Temp)
newTemp (Temp t l) = (t, Temp (t + 1) l)

makeString :: Int -> String
makeString n = "t" ++ show n

newLabel :: Temp -> (Label, Temp)
newLabel (Temp t l) = (Symbol.fromString ("L" ++ show l), (Temp t (l + 1)))

namedLabel :: String -> Label
namedLabel = Symbol.fromString
